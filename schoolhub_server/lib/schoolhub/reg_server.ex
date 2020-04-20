defmodule Schoolhub.RegServer do
  @moduledoc """
  A gen server that manages registering new users.
  A mongooseim admin account adds new users to the postgres database.
  """
  alias Romeo.Connection, as: Conn
  
  require Logger

  use GenServer

  @derive {Inspect, expect: :admin_pw}
  defstruct(
    db_api: Schoolhub.DataManagerMock,
    admin_jid: "admin@localhost",
    admin_pw: "admin",
    admin_conn: nil,
    admin_bound: false,
    admin_ready: false
  )

  ### API functions ###

  @doc false
  def start_link(options) do
    GenServer.start_link(__MODULE__, options, name: __MODULE__)
  end

  @doc false
  def register_user(username, password) do
    GenServer.call(__MODULE__, {:reg_user, string(username), string(password)})
  end

  
  ### Server callbacks ###
  @impl true
  def init(options) do
    state = parse_options(options)
    admin_conn_opts = [jid: state.admin_jid, password: state.admin_pw]
    {:ok, admin_conn} = Conn.start_link(admin_conn_opts)
    {:ok, %{state | admin_conn: admin_conn}}
  end


  @impl true
  def handle_call({:reg_user, username, password}, _from, state = %{db_api: db_api,
								    admin_conn: conn,
								    admin_bound: true,
								    admin_ready: true}) do
    reg_result = %{db_api: db_api,
		   conn: conn,
		   username: username,
		   password: password}
	|> check_username()
	|> check_user_exist()
	|> check_password()
	|> reg_by_admin()
    
    {:reply, reg_result, state}
  end

  
  @impl true
  def handle_info({:resource_bound, _res}, state) do
    {:noreply, %{state | admin_bound: true}}
  end

  @impl true
  def handle_info(:connection_ready, state) do
    {:noreply, %{state | admin_ready: true}}
  end

  @impl true
  def handle_info({:stanza, _stanza}, state) do
    {:noreply, state}
  end


  ### Utility functions ###

  defp parse_options(state = %__MODULE__{}) do
    state
  end
  defp parse_options(options) do
    parse_options(options, %__MODULE__{})
  end
  defp parse_options([], state = %__MODULE__{}) do
    state
  end
  defp parse_options([{:db_api, db_api} | remaining_opts], state) do
    parse_options(remaining_opts, %{state | db_api: db_api})
  end
  defp parse_options([{_key, _value} | remaining_opts] ,state) do
    parse_options(remaining_opts, state)
  end

  defp string(text), do: text |> to_string()


  defp check_username(reg_info = %{username: username}) do
    case username do
      "" ->
	{:error, :username_invalid}
      username_checked ->
	%{reg_info | username: username_checked}
    end
  end

  defp check_user_exist({:error, reason}) do
    {:error, reason}
  end
  defp check_user_exist(reg_info = %{db_api: db_api,
				     username: username}) do
    if db_api.check_user_exist(username) do
      {:error, :user_exists}
    else
      reg_info
    end
  end

  defp check_password({:error, reason}) do
    {:error, reason}
  end
  defp check_password(reg_info = %{password: password}) do
    case password do
      "" ->
	{:error, :password_invalid}
      password_checked ->
	%{reg_info | password: password_checked}
    end
  end

  defp reg_by_admin({:error, reason}) do
    {:error, reason}
  end
  defp reg_by_admin(%{conn: conn,
		      username: username,
		      password: password}) do
    
    :ok = Conn.send(conn, Romeo.Stanza.set_inband_register(username, password))
  end
  
end
