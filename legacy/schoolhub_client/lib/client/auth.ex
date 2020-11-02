defmodule Client.Auth do
  @moduledoc """
  Authentication client module.
  Triggers session start.
  """
  require Logger
  alias Client.RestLib, as: Rest

  use GenServer

  @derive {Inspect, expect: [:password, :salted_pw]}
  defstruct(
    scheme: :http,
    ip: "localhost",
    port: 8080,
    server_opts: [],
    conn: :nil,
    socket: :nil,
    username: "",
    password: "",
    client_first_bare: '',
    salted_pw: '',
    auth_caller: :nil
  )
    
  ### API functions ###

  @doc false
  def start_link(options) do
    GenServer.start_link(__MODULE__, options, name: __MODULE__)
  end

  @doc false
  def auth(username, password) do
    _auth_result = GenServer.call(__MODULE__, {:auth, charlist(username), charlist(password)})
  end


  ### Server callbacks ###
  @impl true
  def init(options) do
    {:ok, parse_options(options)}
  end

  @impl true
  def handle_call({:auth, username, password}, from, state = %{scheme: scheme,
							       ip: ip,
							       port: port,
							       server_opts: opts}) do
    {:ok, conn} = Mint.HTTP1.connect(scheme, ip, port, opts)
    :ok = GenServer.cast(__MODULE__, :client_first)
    {:noreply, %{state |
		 conn: conn,
		 socket: conn.socket,
		 username: username,
		 password: password,
		 auth_caller: from}}
  end

  @impl true
  def handle_cast(:client_first, state = %{conn: conn,
					   username: username}) do
    
    msg = :scramerl.client_first_message(username)
    msg_bare = :scramerl_lib.prune(:"gs2-header", msg)
    {:ok, conn, _request_ref} = Mint.HTTP1.request(conn, "GET", "/auth", [], msg)
    
    {:noreply, %{state |
		 conn: conn,
		 client_first_bare: msg_bare}}
  end

  @impl true
  def handle_cast(%{message: 'server-first-message',
		    salt: salt,
		    "iteration-count": ic,
		    nonce: nonce,
		    str: server_first},
	state = %{conn: conn,
		  password: password,
		  client_first_bare: client_first_bare}) do

    salt = :base64.decode(salt)
    normalized_pw = :stringprep.prepare(password)
    salted_pw = :scramerl_lib.hi(normalized_pw, salt, ic)
    auth_msg = client_first_bare ++ ',' ++ server_first
    ## TODO: client final message without proof is missing here
    ## (in server too, so it is in synchrone but violates standard)
    
    msg = :scramerl.client_final_message(nonce, salted_pw, auth_msg)
    {:ok, conn, _request_ref} = Mint.HTTP1.request(conn, "GET", "/auth", [], msg)
    
    {:noreply, %{state |
		 conn: conn,
		 salted_pw: salted_pw}}
  end

  @impl true
  def handle_cast(%{message: 'server-last-message',
		    "server-error": errorType},
	state) do
    
    result = {:error, errorType}
    {:noreply, %{state |
		 conn: :nil}}

    finish_auth(result, state)
  end
  
  @impl true
  def handle_cast(%{message: 'server-last-message',
		    verifier: server_key},
	state = %{salted_pw: salted_pw}) do

    server_key_from_client =
      salted_pw
      |> reproduce_server_key()

    result =
      {server_key_from_client, server_key}
      |> verify_server_credentials()
    # Server should send server_key hashed with auth_msg, and client encode the same
    # Symmetrical for now, but violates standard
    
    finish_auth(result, state)
  end

  @impl true
  def handle_cast(result = {:error, _reason}, state = %{auth_caller: from}) do
    GenServer.reply(from, result)
    {:noreply, state |> reset_state()}
  end

  @impl true
  def handle_info({transport, socket, http_response}, state = %{conn: conn,
								socket: socket}) do
    
    {:ok, conn, response} = Mint.HTTP1.stream(conn, {transport, socket, http_response})
    {:data, _ref, data}  = :lists.keyfind(:data, 1, response)

    case data do
      "unknown_user" ->
	msg = {:error, 'unknown_user'}
	GenServer.cast(__MODULE__, msg)
      
      correct_data ->
	scram_data = :scramerl_lib.parse(correct_data)
	Logger.debug("Auth client received data: #{inspect(scram_data, pretty: true)}")
	GenServer.cast(__MODULE__, scram_data)
    end
    
    {:noreply, %{state |
		 conn: conn}}
  end

  @impl true
  def handle_info({:tcp_closed, socket}, state) do
    Rest.tcp_closed(socket, state)
  end

  @impl true
  def handle_info({:ssl_closed, socket}, state) do
    Rest.ssl_closed(socket, state)
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
  defp parse_options([{:scheme, scheme} | remaining_opts], state) do
    parse_options(remaining_opts, %{state | scheme: scheme})
  end
  defp parse_options([{:ip, ip} | remaining_opts], state) do
    parse_options(remaining_opts, %{state | ip: ip})
  end
  defp parse_options([{:port, port} | remaining_opts], state) do
    parse_options(remaining_opts, %{state | port: port})
  end
  defp parse_options([{:opts, server_opts} | remaining_opts], state) do
    parse_options(remaining_opts, %{state | server_opts: server_opts})
  end
  defp parse_options([{_key, _value} | remaining_opts] ,state) do
    parse_options(remaining_opts, state)
  end
  
  
  defp charlist(text), do: text |> to_charlist()

  defp reproduce_server_key(salted_pw) do
    _server_key = :crypto.hmac(:sha, salted_pw, "Server Key")
      |> :base64.encode()
      |> charlist()
  end

  defp verify_server_credentials({ckey, skey}) do
    if ckey == skey do
      :authenticated
    else
      {:error, 'server_key_mismatch'}
    end
  end

  defp reset_state(state) do
    %{state |
      username: "",
      password: "",
      client_first_bare: '',
      salted_pw: '',
      auth_caller: :nil}
  end

  defp finish_auth(result, state = %{auth_caller: from}) do
    GenServer.reply(from, result)
    {:noreply, state |> reset_state()}
  end
  
end