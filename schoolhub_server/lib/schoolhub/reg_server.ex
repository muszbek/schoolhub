defmodule Schoolhub.RegServer do
  @moduledoc """
  A gen server that manages registering new users.
  A mongooseim regger account adds new users to the postgres database
  using inband register.
  """

  require Logger

  use GenServer
  
  @scram_default_iteration_count 4096
  @salt_length 16
  @scram_serial_prefix "==SCRAM==,"
  @reg_agent_name "reg_agent"
  @admin_name "admin"

  @derive {Inspect, expect: :admin_pw}
  defstruct(
    db_api: Schoolhub.DataManagerMock,
    xmpp_api: Schoolhub.RomeoMock,
    regger_conn: nil,
    regger_bound: false,
    regger_ready: false
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
  Like remove_user, but removes every reference to the user also, 
  like messages with other users.
  """
  def purge_user(username) do
    GenServer.call(__MODULE__, {:purge_user, string(username)})
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

  @doc """
  Returns the privilege level of the user:
  'student' (default), 'teacher', 'admin'
  """
  def get_user_privilege(username) do
    GenServer.call(__MODULE__, {:get_privilege, string(username)})
  end

  @doc """
  Returns a list of all users with their privileges.
  Only admins are allowed to use this.
  Username is needed as argument to check if admin.
  """
  def get_all_privilege(username) do
    GenServer.call(__MODULE__, {:get_all_privilege, string(username)})
  end

  @doc """
  Following a privilege check on self (only admins are allowed to use this)
  changes the privilage of target.
  """
  def set_user_privilege(self, self, _privilege) do
    {:error, :set_self_privilege}
  end
  def set_user_privilege(self, target, privilege) do
    GenServer.call(__MODULE__, {:set_privilege,
				string(self), string(target), string(privilege)})
  end

  
  ### Server callbacks ###
  @impl true
  def init(options) do
    state = parse_options(options)
    regger_creds = {regger_name, _regger_host, regger_pw} = get_regger_credentials()
    
    {:ok, _reason} = remove_user(regger_name, state.db_api)
    :ok = register_user_db(regger_name, regger_pw, state.db_api)
    
    xmpp_conn = Module.concat(state.xmpp_api, Connection)
    {:ok, regger_conn} =
      regger_creds
      |> regger_conn_opts()
      |> xmpp_conn.start_link()

    {:ok, %{state | regger_conn: regger_conn}}
  end


  @impl true
  def handle_call({:reg_user, username, password}, _from, state = %{regger_bound: true,
								    regger_ready: true}) do
    reg_result = register_user(username, password, state)
    {:reply, reg_result, state}
  end

  @impl true
  def handle_call({:remove_user, username}, _from, state = %{db_api: db_api}) do
    result = remove_user(username, db_api)
    {:reply, result, state}
  end

  @impl true
  def handle_call({:purge_user, username}, _from, state = %{db_api: db_api}) do
    result = purge_user(username, db_api)
    {:reply, result, state}
  end

  @impl true
  def handle_call({:check_user_exist, username}, _from, state = %{db_api: db_api}) do
    result = db_api.check_user_exist(string(username))
    {:reply, result, state}
  end

  @impl true
  def handle_call({:get_privilege, username}, _from, state = %{db_api: db_api}) do
    result = db_api.get_user_privilege(username)
    {:reply, result, state}
  end

  @impl true
  def handle_call({:get_all_privilege, username}, _from, state = %{db_api: db_api}) do
    result = case db_api.get_user_privilege(username) do
	       "admin" -> db_api.get_all_privilege()
	       _other -> {:error, :no_permission}
	     end
    {:reply, result, state}
  end

  @impl true
  def handle_call({:set_privilege, self, target, privilege}, _from,
	state = %{db_api: db_api}) do

    result = set_privilege(self, target, privilege, db_api)
    {:reply, result, state}
  end

  
  @impl true
  def handle_info({:resource_bound, _res}, state) do
    {:noreply, %{state | regger_bound: true}}
  end

  @impl true
  def handle_info(:connection_ready, state) do
    admin_result = create_admin_if_needed(state)
    Logger.info("Creating admin account: #{inspect(admin_result)}")
    
    {:noreply, %{state | regger_ready: true}}
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

  defp get_regger_credentials() do
    regger_name = @reg_agent_name
    regger_host = Application.get_env(:schoolhub, :mongooseim_hostname, "localhost")
    random_pw = random_string(@salt_length)
    
    {regger_name, regger_host, random_pw}
  end

  defp random_string(length) do
    alphabet = Enum.to_list(?a..?z) ++ Enum.to_list(?0..?9)
    Enum.take_random(alphabet, length) |> to_string()
  end

  defp regger_conn_opts({regger_name, regger_host, regger_pw}) do
    [jid: regger_name <> "@" <> regger_host, password: regger_pw]
  end

  defp register_user(username, password, _state = %{db_api: db_api,
						    xmpp_api: xmpp_api,
						    regger_conn: conn}) do
    _reg_result = %{db_api: db_api,
		    xmpp_api: xmpp_api,
		    conn: conn,
		    username: username,
		    password: password}
	|> check_username()
	|> check_user_exist()
	|> check_password()
        |> reg_by_admin()
        |> add_privilege()
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
  defp reg_by_admin(reg_info = %{xmpp_api: xmpp_api,
				 conn: conn,
				 username: username,
				 password: password}) do

    xmpp_conn = Module.concat(xmpp_api, Connection)
    xmpp_stanza = Module.concat(xmpp_api, Stanza)
    :ok = xmpp_conn.send(conn, xmpp_stanza.set_inband_register(username, password))
    reg_info
  end

  defp add_privilege({:error, reason}) do
    {:error, reason}
  end
  defp add_privilege(%{db_api: db_api,
		       username: username}) do
    
    :ok = db_api.add_user_privilege(username)
  end

  defp check_privilege("admin"), do: {:ok, "admin"}
  defp check_privilege("teacher"), do: {:ok, "teacher"}
  defp check_privilege("student"), do: {:ok, "student"}
  defp check_privilege(_other), do: {:error, :wrong_privilege}

  defp can_i_change_privilege("admin"), do: :ok
  defp can_i_change_privilege(_other), do: {:error, :no_permission}

  defp set_privilege(self, target, privilege, db_api) do
    # last clause in with is to check if target exists
    _result = with :ok <- self |> db_api.get_user_privilege() |> can_i_change_privilege(),
                   {:ok, ^privilege} <- privilege |> check_privilege(),
                   {:ok, _priv} <- target |> db_api.get_user_privilege() |> check_privilege()
              do
                :ok = db_api.set_user_privilege(target, privilege)
              else
                err = {:error, _reason} -> err
              end
  end

  defp remove_user(username, db_api) do
    Logger.debug("Removing user: #{inspect(username)}")
    db_api.remove_scram_user(username)
  end

  defp purge_user(username, db_api) do
    Logger.debug("Purging user: #{inspect(username)}")
    db_api.purge_user(username)
  end

  defp check_user_exist_call(username) do
    GenServer.call(__MODULE__, {:check_user_exist, username})
  end

  defp create_admin_if_needed(state) do
    admin_name = @admin_name |> charlist()
    admin_pw = Application.get_env(:schoolhub, :admin_password, "admin") |> charlist()
    
    case register_user(admin_name, admin_pw, state) do
      :ok -> state.db_api.set_user_privilege("admin", "admin")
      {:error, reason} -> reason
    end
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
