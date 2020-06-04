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

    result = case get_course_id(course_name, conn) do
	       err = {:error, _reason} -> err
	       course_id ->
		 do_insert_message(conn, course_id, user, message, nil)
	     end
    {:reply, result, state}
  end

  @impl true
  def handle_call({:post_reply, id, user, course_name, message}, _from,
	state = %{pgsql_conn: conn}) do

    result = case get_course_id(course_name, conn) do
	       err = {:error, _reason} -> err
	       course_id ->
		 origin_query = "SELECT path FROM course_messages WHERE id=$1 AND course=$2;"
		 origin_res = Postgrex.query(conn, origin_query, [id, course_id])

		 case origin_res do
		   {:ok, %{columns: ["path"], num_rows: 0, rows: []}} ->
		     {:error, :origin_not_exist}
		   {:ok, %{columns: ["path"], num_rows: 1, rows: [[origin_path]]}} ->
		     do_insert_message(conn, course_id, user, message, origin_path)
		 end
	     end
    {:reply, result, state}
  end

  @impl true
  def handle_call({:get_single_message, id, course_name}, _from,
	state = %{pgsql_conn: conn}) do

    result = case get_course_id(course_name, conn) do
	       err = {:error, _reason} -> err
	       course_id ->
		 message_query =
		   "SELECT author, subpath(path, -2, -1), message, created_at, pinned " <>
		   "FROM course_messages WHERE id=$1 AND course=$2;"
		 message_res = Postgrex.query(conn, message_query, [id, course_id])

		 case message_res do
		   {:ok, %{columns: ["author", "subpath", "message", "created_at", "pinned"],
			    command: :select, num_rows: 0, rows: []}} ->
		     {:error, :message_not_exist}
		     
                   {:ok, %{columns: ["author", "subpath", "message", "created_at", "pinned"],
			   command: :select, num_rows: 1,
			    rows: [message_response]}} ->

		     pack_message_json(id, course_name, message_response)
		 end
	     end
    {:reply, result, state}
  end


  ### Utility functions ###
  
  defp string(text), do: text |> to_string()

  defp get_course_id(course_name, conn) do
    query_id = "SELECT id FROM courses WHERE name LIKE $1;"
    id_result = Postgrex.query(conn, query_id, [string(course_name)])

    _result = case id_result do
		{:ok, %{columns: ["id"], command: :select, num_rows: 1, rows: [[id]]}} ->
		  id
	       
	        {:ok, %{columns: ["id"], command: :select, num_rows: 0, rows: []}} ->
		  {:error, :course_not_exist}
	      end
  end

  defp do_insert_message(conn, course_id, user, message, origin_path) do
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

  defp get_path(nil, id), do: id |> string()
  defp get_path(origin_path, id), do: string(origin_path) <> "." <> string(id)

  defp get_timestamp(timestamp), do: timestamp |> string()

  defp get_ancestor(""), do: nil
  defp get_ancestor(ancestor), do: ancestor |> String.to_integer()

  defp pack_message_json(id, course_name,
	_message_response = [author, ancestor, message, timestamp, is_pinned]) do

    %{id: id,
      course: course_name,
      author: author,
      ancestor: ancestor |> get_ancestor(),
      message: message,
      timestamp: timestamp |> get_timestamp(),
      pinned: is_pinned}
  end

end
