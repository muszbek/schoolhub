defmodule Schoolhub.DataManager do
  @moduledoc """
  API for accessing the Postgres database.
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
  Fetching the scram data entry associated with username in the database.
  The scram data entry is a composition of different hashed values, decoded in Schoolhub.Auth.
  """
  def get_scram_pw(username) do
    GenServer.call(__MODULE__, {:get_pw, username})
  end

  @doc """
  Returns true if an entry with 'username' already exists within the database,
  otherwise false.
  """
  def check_user_exist(username) do
    _does_exist = GenServer.call(__MODULE__, {:check_user_exist, username})
  end

  @doc """
  Adds the same database user entry as mongooseim.
  The caller should construct the scram 'pass_details' (normally Schoolhub.RegServer)
  """
  def add_scram_user(username, pass_details) do
    GenServer.call(__MODULE__, {:add_scram_user, username, pass_details})
  end

  @doc """
  Removes the database entry associated with 'username'.
  """
  def remove_scram_user(username) do
    GenServer.call(__MODULE__, {:remove_scram_user, username})
  end

  @doc """
  Removes the database entry and every reference associated with 'username'.
  """
  def purge_user(username) do
    GenServer.call(__MODULE__, {:purge_user, username})
  end

  @doc """
  Fetches XMPP message archive between two users.
  """
  def get_archive(self, partner, limit) do
    GenServer.call(__MODULE__, {:get_archive, self, partner, limit})
  end

  @doc """
  Adds user to user_privileges table when
  creating new user through mongooseim.
  Default privilege is 'student', an admin has to change it.
  """
  def add_user_privilege(username) do
    GenServer.call(__MODULE__, {:add_privilege, username})
  end

  @doc """
  Interrogates the privilege level of the user.
  'student' -> normal user with restricted privileges, default
  'teacher' -> can create courses
  'admin' -> can create courses and changes privileges of others
  """
  def get_user_privilege(username) do
    GenServer.call(__MODULE__, {:get_privilege, username})
  end

  @doc """
  Sets the privilege level of the user.
  The RegServer controls whether the user has right to do this.
  """
  def set_user_privilege(username, privilege) do
    GenServer.call(__MODULE__, {:set_privilege, username, privilege})
  end
    

  ### Server callbacks ###
  
  @impl true
  def init(:ok) do
    args = Application.get_env(:schoolhub, :postgres_address)
    
    {:ok, pid} = Postgrex.start_link(args)
    {:ok, %__MODULE__{pgsql_conn: pid}}
  end

  @impl true
  def handle_call({:get_pw, username}, _from, state = %{pgsql_conn: conn}) do
    query_text = "SELECT pass_details FROM users WHERE username LIKE $1"
    {:ok, data} = Postgrex.query(conn, query_text, [string(username)])

    case data.rows do
      [] ->
	Logger.debug("#{inspect(username)} is not present in database")
        {:reply, :nil, state}
      [[result]] ->
	Logger.debug("Fetching scram data from database for username #{inspect(username)}: "
	  <> "#{inspect(result)}")
        {:reply, result, state}
    end
  end

  @impl true
  def handle_call({:check_user_exist, username}, _from, state = %{pgsql_conn: conn}) do
    query_text = "SELECT username FROM users WHERE username LIKE $1"
    {:ok, data} = Postgrex.query(conn, query_text, [string(username)])

    case data.rows do
      [] ->
        {:reply, false, state}
      [[^username]] ->
        {:reply, true, state}
    end
  end

  @impl true
  def handle_call({:add_scram_user, username, pass_details}, _from, state = %{pgsql_conn: conn}) do
    query_text = "INSERT INTO users (username, password, pass_details) VALUES ($1, '', $2);"
    result = Postgrex.query(conn, query_text, [string(username), string(pass_details)])

    case result do
      {:ok, %{columns: nil, command: :insert, messages: [], num_rows: 1, rows: nil}} ->
	{:reply, :ok, state}
      {:error, %Postgrex.Error{postgres: %{code: :unique_violation,
					   constraint: "users_pkey",
					   severity: "ERROR",
					   table: "users"}}} ->
	{:reply, :user_exists, state}
    end
  end

  @impl true
  def handle_call({:remove_scram_user, username}, _from, state = %{pgsql_conn: conn}) do
    result = remove_user(username, conn)
    {:reply, {:ok, result}, state}
  end

  @impl true
  def handle_call({:purge_user, username}, _from, state = %{pgsql_conn: conn}) do
    query_text = "DELETE FROM mam_message WHERE remote_bare_jid LIKE $1;"
    {:ok, %{command: :delete}} = Postgrex.query(conn, query_text, [string(username)])
    result = remove_user(username, conn)
    {:reply, {:ok, result}, state}
  end

  @impl true
  def handle_call({:get_archive, self, partner, limit}, _from, state = %{pgsql_conn: conn}) do
    limit_query = case limit do
		    :all -> ""
		    number when is_integer(number) and number > 0 ->
		      "LIMIT " <> string(number)
		  end
    id_query = "SELECT id FROM mam_server_user WHERE user_name LIKE $1"
    query_text = "SELECT direction, search_body FROM mam_message WHERE user_id = (" <>
      id_query <> ") AND remote_bare_jid LIKE $2 " <> limit_query <> ";"
    
    {:ok, data} = Postgrex.query(conn, query_text, [string(self), string(partner)])
    {:reply, data.rows, state}
  end

  @impl true
  def handle_call({:add_privilege, username}, _from, state = %{pgsql_conn: conn}) do
    query_text = "INSERT INTO user_privileges (username, permission) VALUES ($1, 'student');"
    result = Postgrex.query(conn, query_text, [string(username)])
    {:ok, %{columns: nil, command: :insert, messages: [], num_rows: 1, rows: nil}} = result
    {:reply, :ok, state}
  end

  @impl true
  def handle_call({:get_privilege, username}, _from, state = %{pgsql_conn: conn}) do
    query_text = "SELECT permission FROM user_privileges WHERE username LIKE $1;"
    result = Postgrex.query(conn, query_text, [string(username)])
    
    permission = case result do
		   {:ok, %{columns: ["permission"], num_rows: 1, rows: [[perm]]}} ->
		     perm
		   {:ok, %{columns: ["permission"], num_rows: 0, rows: []}} ->
		     :nil
		 end
    
    {:reply, permission, state}
  end

  @impl true
  def handle_call({:set_privilege, username, privilege}, _from, state = %{pgsql_conn: conn}) do
    query_text = "UPDATE user_privileges SET permission = $2 WHERE username LIKE $1;"
    query_result = Postgrex.query(conn, query_text, [string(username), string(privilege)])

    result = case query_result do
	       {:ok, %{command: :update, num_rows: 1, rows: nil}} -> :ok
	       {:ok, %{command: :update, num_rows: 0, rows: nil}} -> {:error, :user_not_exist}
	     end
    
    {:reply, result, state}
  end

  
  ### Utility functions ###
  
  defp string(text), do: text |> to_string()

  defp remove_user(username, conn) do
    sub_query_mam_id = "SELECT id FROM mam_server_user WHERE user_name LIKE $1"
    query_delete_mam = "DELETE FROM mam_message WHERE user_id = (" <> sub_query_mam_id <> ") ;"
    {:ok, %{command: :delete}} = Postgrex.query(conn, query_delete_mam, [string(username)])

    query_delete_mam_user = "DELETE FROM mam_server_user WHERE user_name LIKE $1 ;"
    {:ok, %{command: :delete}} = Postgrex.query(conn, query_delete_mam_user, [string(username)])

    :ok = remove_privilege(username, conn)
    
    query_text = "DELETE FROM users WHERE username LIKE $1;"
    result = Postgrex.query(conn, query_text, [string(username)])

    case result do
      {:ok, %{columns: nil, command: :delete, messages: [], num_rows: 1, rows: nil}} ->
	:user_removed
      {:ok, %{columns: nil, command: :delete, messages: [], num_rows: 0, rows: nil}} ->
	:user_not_existed
    end
  end

  defp remove_privilege(username, conn) do
    query_text = "DELETE FROM user_privileges WHERE username LIKE $1 ;"
    {:ok, %{command: :delete}} = Postgrex.query(conn, query_text, [string(username)])
    :ok
  end
  
end
