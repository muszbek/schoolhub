defmodule Client.Auth do
  @moduledoc """
  Authentication client module.
  Triggers session start.
  """
  require Logger

  use GenServer

  @derive {Inspect, expect: [:password, :salted_pw]}
  defstruct(
    scheme: :http,
    ip: "localhost",
    port: 8080,
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
  def start_link([]) do
    GenServer.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  @doc false
  def auth(username, password) do
    _auth_result = GenServer.call(__MODULE__, {:auth, charlist(username), charlist(password)})
  end


  ### Server callbacks ###
  @impl true
  def init(:ok) do
    state = %__MODULE__{}
    
    {:ok, state}
  end

  @impl true
  def handle_call({:auth, username, password}, from, state) do
    {:ok, conn} = Mint.HTTP.connect(state.scheme, state.ip, state.port)
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
    {:ok, conn, _request_ref} = Mint.HTTP.request(conn, "GET", "/auth", [], msg)
    
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
    
    msg = :scramerl.client_final_message(nonce, salted_pw, auth_msg)
    {:ok, conn, _request_ref} = Mint.HTTP.request(conn, "GET", "/auth", [], msg)
    
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
    
    {:ok, conn, response} = Mint.HTTP.stream(conn, {transport, socket, http_response})
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
  def handle_info({:tcp_closed, socket}, state = %{socket: socket}) do
    Logger.debug("TCP closed for socket #{inspect(socket)}")
    {:noreply, %{state |
		 conn: :nil,
		 socket: :nil} |> reset_state()}
  end

  @impl true
  def handle_info({:tcp_closed, socket}, state = %{socket: _other_socket}) do
    ## This message does not affect the current authentication session.
    Logger.debug("TCP closed for socket #{inspect(socket)}")
    {:noreply, state}
  end
  

  
  ### Utility functions ###
  
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

# username:
# test_user

# password:
# test_pw (not in db)

# pass_details:
# ==SCRAM==,jv1SCgihx+Q2yj6PggxUZPbmfp4=,r+T1xjRnDwpUPoC/EwOXA+Jjt2Y=,iCgKQkjMSgfZgjh06UMZzg==,4096
