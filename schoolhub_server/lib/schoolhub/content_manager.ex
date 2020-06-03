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


  ### Server callbacks ###
  
  @impl true
  def init(:ok) do
    args = Application.get_env(:schoolhub, :postgres_address)
    
    {:ok, pid} = Postgrex.start_link(args)
    {:ok, %__MODULE__{pgsql_conn: pid}}
  end


  @impl true
  def handle_call({:post_message, user, course_name, message}, _from,
	state = %{pgsql_conn: conn}) do

    result = case get_course_id(course_name, conn) do
	       err = {:error, _reason} -> err
	       course_id ->
		 query_message = "INSERT INTO course_messages " <>
		   "(course, author, message, pinned) VALUES " <>
		   "($1, $2, $3, true) RETURNING id;"
		 message_res = Postgrex.query(conn, query_message, [course_id, user, message])
		 {:ok, %{columns: ["id"], command: :insert, rows: [[message_id]]}} = message_res
		 
		 query_path = "UPDATE course_messages SET path = $2 WHERE id = $1;"
		 _path_res = Postgrex.query(conn, query_path, [message_id, message_id |> string()])
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

end
