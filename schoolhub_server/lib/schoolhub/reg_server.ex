defmodule Schoolhub.RegServer do
  @moduledoc """
  A gen server that manages registering new users.
  A mongooseim admin account adds new users to the postgres database.
  """

  require Logger

  use GenServer
  
  @scram_default_iteration_count 4096
  @salt_length 16
  @scram_serial_prefix "==SCRAM==,"

  @derive {Inspect, expect: :admin_pw}
  defstruct(
    db_api: Schoolhub.DataManagerMock,
    xmpp_api: Schoolhub.RomeoMock,
    admin_conn: nil,
    admin_bound: false,
    admin_ready: false
  )

  ### API functions ###

  @doc false
  def start_link(options) do
    GenServer.start_link(__MODULE__, options, name: __MODULE__)
  end

  @doc """
  Adds a new user entry into the database indirectly,
  through an admin xmpp user of mongooseim.
  """
  def register_user(username, password) do
    GenServer.call(__MODULE__, {:reg_user, charlist(username), charlist(password)})
  end

  @doc """
  Calls the database API to remove a user directly.
  Succeeds if the user is removed, or if it was not there in the first place.
  """
  def remove_user(username) do
    GenServer.call(__MODULE__, {:remove_user, string(username)})
  end

  @doc """
  Effectively removes the user (if exists) and adds it again with updated password.
  """
  def change_user_pw(username, password) do
    case check_password(%{password: charlist(password)}) do
      result = {:error, :password_invalid} ->
	result
      _ ->
	if check_user_exist_call(username) do
	  {:ok, :user_removed} = remove_user(username)
	  :ok = register_user(username, password)
	else
	  {:error, :user_not_exist}
	end
    end
  end

  
  ### Server callbacks ###
  @impl true
  def init(options) do
    state = parse_options(options)
    admin_creds = {admin_name, _admin_host, admin_pw} = get_admin_credentials()
    
    {:ok, _reason} = remove_user(admin_name, state.db_api)
    :ok = register_user_db(admin_name, admin_pw, state.db_api)
    
    xmpp_conn = Module.concat(state.xmpp_api, Connection)
    {:ok, admin_conn} =
      admin_creds
      |> admin_conn_opts()
      |> xmpp_conn.start_link()
    
    {:ok, %{state | admin_conn: admin_conn}}
  end


  @impl true
  def handle_call({:reg_user, username, password}, _from, state = %{db_api: db_api,
								    xmpp_api: xmpp_api,
								    admin_conn: conn,
								    admin_bound: true,
								    admin_ready: true}) do
    reg_result = %{db_api: db_api,
		   xmpp_api: xmpp_api,
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
  def handle_call({:remove_user, username}, _from, state = %{db_api: db_api}) do
    result = remove_user(username, db_api)
    {:reply, result, state}
  end

  @impl true
  def handle_call({:check_user_exist, username}, _from, state = %{db_api: db_api}) do
    result = db_api.check_user_exist(string(username))
    {:reply, result, state}
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
  defp parse_options([{:xmpp_api, xmpp_api} | remaining_opts], state) do
    parse_options(remaining_opts, %{state | xmpp_api: xmpp_api})
  end
  defp parse_options([{_key, _value} | remaining_opts] ,state) do
    parse_options(remaining_opts, state)
  end

  defp string(text), do: text |> to_string()
  defp charlist(text), do: text |> to_charlist()

  defp get_admin_credentials() do
    admin_name = Application.get_env(:schoolhub, :reg_admin_name, "admin")
    admin_host = Application.get_env(:schoolhub, :mongooseim_hostname, "localhost")
    admin_pw = Application.get_env(:schoolhub, :reg_admin_pw, "admin")
    
    {admin_name, admin_host, admin_pw}
  end

  defp admin_conn_opts({admin_name, admin_host, admin_pw}) do
    [jid: admin_name <> "@" <> admin_host, password: admin_pw]
  end

  defp register_user_db(username, password, db_api) do
    ## This is only used for the admin registration in init.
    ## Better not to duplicate the Mongooseim registration process
    ## when it is not absolutely necessary.
    pass_details = password_to_scram(to_charlist(password))
    Logger.debug("Inserting new user: #{inspect(username)} ; #{inspect(pass_details)}")
    db_api.add_scram_user(username, pass_details)
  end

  defp password_to_scram(password) do
    password_to_scram(password, @scram_default_iteration_count)
  end

  
  defp check_username(reg_info = %{username: username}) do
    case :stringprep.prepare(username) do
      '' ->
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
    if db_api.check_user_exist(string(username)) do
      {:error, :user_exists}
    else
      reg_info
    end
  end

  defp check_password({:error, reason}) do
    {:error, reason}
  end
  defp check_password(reg_info = %{password: password}) do
    case :stringprep.prepare(password) do
      '' ->
	{:error, :password_invalid}
      password_checked ->
	%{reg_info | password: password_checked}
    end
  end

  defp reg_by_admin({:error, reason}) do
    {:error, reason}
  end
  defp reg_by_admin(%{xmpp_api: xmpp_api,
		      conn: conn,
		      username: username,
		      password: password}) do

    xmpp_conn = Module.concat(xmpp_api, Connection)
    xmpp_stanza = Module.concat(xmpp_api, Stanza)
    :ok = xmpp_conn.send(conn, xmpp_stanza.set_inband_register(username, password))
  end

  def remove_user(username, db_api) do
    Logger.debug("Removing user: #{inspect(username)}")
    db_api.remove_scram_user(username)
  end

  defp check_user_exist_call(username) do
    GenServer.call(__MODULE__, {:check_user_exist, username})
  end


  ## Ported from erlang code of mongooseim
  
  defp password_to_scram(password, iteration_count) do
    salt = :crypto.strong_rand_bytes(@salt_length)
    server_stored_keys = password_to_scram(password, salt, iteration_count, :sha)
    result_list = server_stored_keys ++ [salt: :base64.encode(salt),
					 iteration_count: iteration_count |> to_string()]
    serialize(Enum.into(result_list, %{}))
  end
  
  defp password_to_scram(password, salt, iteration_count, hash_type) do
    salted_password = salted_password(hash_type, password, salt, iteration_count)
    stored_key = stored_key(hash_type, client_key(hash_type, salted_password))
    server_key = server_key(hash_type, salted_password)
    
    [server_key: :base64.encode(server_key),
     stored_key: :base64.encode(stored_key)]
  end

  defp salted_password(_hash_type, password, salt, iteration_count) do
    normalized_pw = :stringprep.prepare(password)
    _salted_pw = :scramerl_lib.hi(normalized_pw, salt, iteration_count)
  end

  defp client_key(hash_type, salted_password) do
    :crypto.hmac(hash_type, salted_password, "Client Key")
  end
  
  defp stored_key(hash_type, client_key) do
    :crypto.hash(hash_type, client_key)
  end

  defp server_key(hash_type, salted_password) do
    :crypto.hmac(hash_type, salted_password, "Server Key")
  end

  defp serialize(%{server_key: server_key,
		   stored_key: stored_key,
		   salt: salt,
		   iteration_count: ic}) do
    
    @scram_serial_prefix <> stored_key <> "," <> server_key <> "," <> salt <> "," <> ic
  end
  
end
