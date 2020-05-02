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
  Fetches XMPP message archive between two users.
  """
  def get_archive(self, partner) do
    GenServer.call(__MODULE__, {:get_archive, self, partner})
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
    query_text = "DELETE FROM users WHERE username LIKE $1;"
    result = Postgrex.query(conn, query_text, [string(username)])

    case result do
      {:ok, %{columns: nil, command: :delete, messages: [], num_rows: 1, rows: nil}} ->
	{:reply, {:ok, :user_removed}, state}
      {:ok, %{columns: nil, command: :delete, messages: [], num_rows: 0, rows: nil}} ->
	{:reply, {:ok, :user_not_existed}, state}
    end
  end

  @impl true
  def handle_call({:get_archive, self, partner}, _from, state = %{pgsql_conn: conn}) do
    id_query = "SELECT id FROM mam_server_user WHERE user_name LIKE $1"
    query_text = "SELECT direction, search_body FROM mam_message WHERE user_id = (" <>
      id_query <> ") AND remote_bare_jid LIKE $2;"
    {:ok, data} = Postgrex.query(conn, query_text, [string(self), string(partner)])
    {:reply, data.rows, state}
  end

  
  ### Utility functions ###
  
  defp string(text), do: text |> to_string()
  
end
