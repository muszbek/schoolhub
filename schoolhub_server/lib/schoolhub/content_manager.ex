defmodule Schoolhub.ContentManager do
  @moduledoc """
  API for accessing the Postgres database.
  Concerns course content, generic data are handled by Schoolhub.DataManager.
  """
  require Logger

  use GenServer

  defstruct(
    pgsql_conn: :nil
  )

  ### API functions ###

  @doc false
  def start_link([name: name]) do
    GenServer.start_link(name, :ok, name: __MODULE__)
  end

  @doc false
  def start_link([]) do
    GenServer.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  @doc """
  Add a new message to the specified course on the root level.
  """
  def post_message(user, course_name, message) do
    GenServer.call(__MODULE__, {:post_message, user, course_name, message})
  end

  @doc """
  Add a new message to the specified course as a reply to another message.
  """
  def post_reply(id, user, course_name, message) do
    GenServer.call(__MODULE__, {:post_reply, id, user, course_name, message})
  end

  @doc """
  Retrieves a message as json.
  """
  def get_single_message(id, course_name) do
    GenServer.call(__MODULE__, {:get_single_message, id, course_name})
  end

  @doc """
  Removes a single message.
  If the message has descendants (replies), it leaves an empty placeholder.
  """
  def delete_single_message(id, course_name) do
    GenServer.call(__MODULE__, {:delete_single_message, id, course_name})
  end

  @doc """
  Modifies a single message.
  Leaves a trace of who modified it last.
  """
  def modify_single_message(id, course_name, user, message) do
    GenServer.call(__MODULE__, {:modify_single_message, id, course_name, user, message})
  end

  @doc """
  Retrieves a specified number of root level messages.
  Pinned ones are on the top.
  """
  def get_root_messages(course_name, number) do
    GenServer.call(__MODULE__, {:get_root_messages, course_name, number})
  end

  @doc """
  Retrieves a specified number of replies for the specified root message.
  """
  def get_replies(id, course_name, number) do
    GenServer.call(__MODULE__, {:get_replies, id, course_name, number})
  end

  @doc """
  Sets the pinned flag of the root message.
  Pinned messages are pushed to top in order.
  """
  def pin_message(id, course_name, pinned) do
    GenServer.call(__MODULE__, {:pin_message, id, course_name, pinned})
  end

  @doc """
  Removes root message together with all replies.
  """
  def delete_root_message(id, course_name) do
    GenServer.call(__MODULE__, {:delete_root_message, id, course_name})
  end


  ### Server callbacks ###
  
  @impl true
  def init(:ok) do
    args = Application.get_env(:schoolhub, :postgres_address) ++ [types: Schoolhub.PostgrexTypes]
    
    {:ok, pid} = Postgrex.start_link(args)
    {:ok, %__MODULE__{pgsql_conn: pid}}
  end


  @impl true
  def handle_call({:post_message, user, course_name, message}, _from,
	state = %{pgsql_conn: conn}) do

    result = {conn, course_name}
      |> get_course_id()
      |> (&(Tuple.append(&1, nil))).()
      |> do_insert_message(user, message)
    
    {:reply, result, state}
  end

  @impl true
  def handle_call({:post_reply, id, user, course_name, message}, _from,
	state = %{pgsql_conn: conn}) do

    result = {conn, course_name}
      |> get_course_id()
      |> get_origin_path(id)
      |> do_insert_message(user, message)
    
    {:reply, result, state}
  end

  @impl true
  def handle_call({:get_single_message, id, course_name}, _from,
	state = %{pgsql_conn: conn}) do

    result = {conn, course_name}
      |> get_course_id()
      |> do_get_single_message(id, course_name)
    
    {:reply, result, state}
  end
  
  @impl true
  def handle_call({:modify_single_message, id, course_name, user, message}, _from,
	state = %{pgsql_conn: conn}) do

    signed_message = Map.put(message, :modified, user)
    result = {conn, course_name}
      |> get_course_id()
      |> do_modify_message(id, signed_message)
    
    {:reply, result, state}
  end
  
  @impl true
  def handle_call({:delete_single_message, id, course_name}, _from,
	state = %{pgsql_conn: conn}) do

    result = {conn, course_name}
      |> get_course_id()
      |> do_delete_single_message(id)
    
    {:reply, result, state}
  end

  @impl true
  def handle_call({:get_root_messages, course_name, number}, _from,
	state = %{pgsql_conn: conn}) do

    result = {conn, course_name}
      |> get_course_id()
      |> do_get_root_messages(course_name, number)
    
    {:reply, result, state}
  end

  @impl true
  def handle_call({:get_replies, id, course_name, number}, _from,
	state = %{pgsql_conn: conn}) do

    result = {conn, course_name}
      |> get_course_id()
      |> do_get_replies(id, course_name, number)
    
    {:reply, result, state}
  end

  @impl true
  def handle_call({:pin_message, id, course_name, pinned}, _from,
	state = %{pgsql_conn: conn}) do

    result = {conn, course_name}
      |> get_course_id()
      |> do_set_pinned(id, pinned)
    
    {:reply, result, state}
  end

  @impl true
  def handle_call({:delete_root_message, id, course_name}, _from,
	state = %{pgsql_conn: conn}) do

    result = {conn, course_name}
      |> get_course_id()
      |> do_delete_root_message(id)
    
    {:reply, result, state}
  end


  ### Utility functions ###
  
  defp string(text), do: text |> to_string()

  defp get_course_id({conn, course_name}) do
    query_id = "SELECT id FROM courses WHERE name LIKE $1;"
    id_result = Postgrex.query(conn, query_id, [string(course_name)])

    case id_result do
      {:ok, %{columns: ["id"], command: :select, num_rows: 1, rows: [[id]]}} ->
	{conn, id}
      
      {:ok, %{columns: ["id"], command: :select, num_rows: 0, rows: []}} ->
	{:error, :course_not_exist}
    end
  end

  defp get_origin_path({:error, reason}, _), do: {:error, reason}
  defp get_origin_path({conn, course_id}, id) do
    origin_query = "SELECT path FROM course_messages WHERE id=$1 AND course=$2;"
    origin_res = Postgrex.query(conn, origin_query, [id, course_id])

    case origin_res do
      {:ok, %{columns: ["path"], num_rows: 0, rows: []}} ->
	{:error, :origin_not_exist}
      {:ok, %{columns: ["path"], num_rows: 1, rows: [[origin_path]]}} ->
	{conn, course_id, origin_path}
    end
  end

  defp do_insert_message({:error, reason}, _, _), do: {:error, reason}
  defp do_insert_message({:error, reason, nil}, _, _), do: {:error, reason}
  defp do_insert_message({conn, course_id, origin_path}, user, message) do
    query_message = "INSERT INTO course_messages " <>
      "(course, author, message, pinned) VALUES " <>
      "($1, $2, $3, false) RETURNING id;"
    message_res = Postgrex.query(conn, query_message, [course_id, user, message])
    {:ok, %{columns: ["id"], command: :insert, rows: [[message_id]]}} = message_res
    
    query_path = "UPDATE course_messages SET path = $2 WHERE id = $1;"
    path_data = get_path(origin_path, message_id)
    path_res = Postgrex.query(conn, query_path, [message_id, path_data])
    {:ok, %{command: :update, num_rows: 1, rows: nil}} = path_res
    {:ok, message_id}
  end

  defp do_get_single_message({:error, reason}, _, _), do: {:error, reason}
  defp do_get_single_message({conn, course_id}, id, course_name) do
    message_query =
      "SELECT author, subpath(path, -2, -1) AS \"ancestor\", message, created_at, pinned " <>
      "FROM course_messages WHERE id=$1 AND course=$2;"
    message_res = Postgrex.query(conn, message_query, [id, course_id])
    
    case message_res do
      {:ok, %{columns: ["author", "ancestor", "message", "created_at", "pinned"],
	       command: :select, num_rows: 0, rows: []}} ->
	
	{:error, :message_not_exist}
	
      {:ok, %{columns: ["author", "ancestor", "message", "created_at", "pinned"],
	       command: :select, num_rows: 1, rows: [message_response]}} ->
	
	pack_message_json(id, course_name, message_response)
    end
  end

  defp get_path(nil, id), do: id |> string()
  defp get_path(origin_path, id), do: string(origin_path) <> "." <> string(id)

  defp get_timestamp(timestamp), do: timestamp |> string()

  defp get_ancestor(""), do: nil
  defp get_ancestor(ancestor), do: ancestor |> String.to_integer()

  defp pack_message_json(id, course_name,
	_message_response = [author, ancestor, message, timestamp, is_pinned]) do

    %Schoolhub.Post{id: id,
		    course: course_name,
		    author: author,
		    ancestor: ancestor |> get_ancestor(),
		    message: message,
		    timestamp: timestamp |> get_timestamp(),
		    pinned: is_pinned}
  end

  defp do_modify_message({:error, reason}, _, _), do: {:error, reason}
  defp do_modify_message({conn, course_id}, id, message) do
    modify_query = "UPDATE course_messages SET message = $3 WHERE id=$1 AND course=$2;"
    modify_result = Postgrex.query(conn, modify_query, [id, course_id, message])
    case modify_result do
      {:ok, %{command: :update, num_rows: 1, rows: nil}} -> :ok
      {:ok, %{command: :update, num_rows: 0, rows: nil}} -> {:error, :message_not_exist}
    end
  end

  defp do_delete_single_message({:error, reason}, _), do: {:error, reason}
  defp do_delete_single_message({conn, course_id}, id) do
    case has_descendants(conn, id, course_id) do
      false ->
	delete_query = "DELETE FROM course_messages WHERE id=$1 AND course=$2;"
        {:ok, %{command: :delete}} = Postgrex.query(conn, delete_query, [id, course_id])
	:ok
      
      true ->
	do_modify_message({conn, course_id}, id, "<deleted>")
    end
  end
  
  defp has_descendants(conn, id, course_id) do
    subquery_text = "SELECT path FROM course_messages WHERE id=$1 AND course=$2"
    descendants_query = "SELECT id FROM course_messages WHERE subpath(path, 0, -1) <@ (" <>
      subquery_text <> ");"
    descendants = Postgrex.query(conn, descendants_query, [id, course_id])

    case descendants do
      {:ok, %{command: :select, num_rows: 0, rows: []}} -> false
      {:ok, %{command: :select, num_rows: _not_zero}} -> true
    end
  end

  
  defp do_get_root_messages({:error, reason}, _, _), do: {:error, reason}
  defp do_get_root_messages({conn, course_id}, course_name, number) do
    query_text = "SELECT id, author, message, created_at, pinned, " <>
      "(SELECT COUNT(*) FROM course_messages WHERE subpath(path, 0, -1) <@ " <>
      "\"outer\".id::text::ltree) AS replies FROM course_messages AS \"outer\"" <>
      "WHERE course=$1 AND nlevel(path)=1 ORDER BY pinned DESC, created_at DESC LIMIT $2;"
    result = Postgrex.query(conn, query_text, [course_id, number])

    {:ok, %{columns: ["id", "author", "message", "created_at", "pinned", "replies"],
	    command: :select, rows: messages}} = result
    
    pack_root_messages(course_name, messages)
  end

  defp pack_root_messages(course_name, messages) do
    Enum.map(messages, &(pack_root_message_json(course_name, &1)))
  end

  defp pack_root_message_json(course_name,
	_message = [id, author, content, timestamp, pinned, replies]) do

    %Schoolhub.Post{id: id,
		    course: course_name,
		    author: author,
		    message: content,
		    timestamp: timestamp |> get_timestamp(),
		    pinned: pinned,
		    replies: replies}
  end


  defp do_get_replies({:error, reason}, _, _, _), do: {:error, reason}
  defp do_get_replies({conn, course_id}, id, course_name, number) do
    query_text = "SELECT id, author, subpath(path, -2, -1) AS \"ancestor\", " <>
      "message, created_at, pinned FROM course_messages WHERE course=$2 AND " <>
      "path <@ $1::ltree ORDER BY path, created_at LIMIT $3;"
    result = Postgrex.query(conn, query_text, [id |> string(), course_id, number])

    {:ok, %{columns: ["id", "author", "ancestor", "message", "created_at", "pinned"],
	    command: :select, rows: replies}} = result

    pack_replies(course_name, replies)
  end

  defp pack_replies(course_name, replies) do
    Enum.map(replies, &(pack_reply_json(course_name, &1)))
  end

  defp pack_reply_json(course_name,
	_reply = [id, author, ancestor, content, timestamp, pinned]) do

    %Schoolhub.Post{id: id,
		    course: course_name,
		    author: author,
		    ancestor: ancestor |> get_ancestor(),
		    message: content,
		    timestamp: timestamp |> get_timestamp(),
		    pinned: pinned}
  end


  defp do_set_pinned({:error, reason}, _, _), do: {:error, reason}
  defp do_set_pinned({conn, course_id}, id, pinned) do
    query_text = "UPDATE course_messages SET pinned = $3 WHERE id=$1 AND course=$2 " <>
      "AND nlevel(path)=1;"
    pin_result = Postgrex.query(conn, query_text, [id, course_id, pinned])

    case pin_result do
      {:ok, %{command: :update, num_rows: 1, rows: nil}} -> :ok
      {:ok, %{command: :update, num_rows: 0, rows: nil}} -> {:error, :message_not_exist}
    end
  end


  defp do_delete_root_message({:error, reason}, _), do: {:error, reason}
  defp do_delete_root_message({conn, course_id}, id) do
    query_text = "DELETE FROM course_messages WHERE course = $2 AND path <@ $1::ltree;"
    {:ok, %{command: :delete}} = Postgrex.query(conn, query_text, [id |> string(), course_id])
    :ok
  end

end
