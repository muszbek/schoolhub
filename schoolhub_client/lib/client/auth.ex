defmodule Client.Auth do
  @moduledoc """
  Authentication client module.
  Triggers session start.
  """
  require Logger

  use GenServer

  @derive {Inspect, expect: [:password]}
  defstruct(
    scheme: :http,
    ip: "localhost",
    port: 8080,
    conn: :nil,
    socket: :nil,
    username: "",
    password: "",
    auth_stage: :not_started
  )
    
  ### API functions ###

  @doc false
  def start_link([]) do
    GenServer.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  @doc false
  def auth(username, password) do
    auth_result = GenServer.call(__MODULE__, {:auth, charlist(username), charlist(password)})
    Logger.debug(inspect(auth_result))
  end

  ### Server callbacks ###
  @impl true
  def init(:ok) do
    state = %__MODULE__{}
    
    {:ok, state}
  end

  # Message handling in sequential order, not grouped together by type:
  
  @impl true
  def handle_call({:auth, username, password}, _from, state) do
    {:ok, conn} = Mint.HTTP.connect(state.scheme, state.ip, state.port)
    :ok = GenServer.cast(__MODULE__, :client_first)
    {:reply, :ok, %{state | conn: conn, socket: conn.socket,
		    username: username, password: password}}
  end

  @impl true
  def handle_cast(:client_first, state = %{username: username, conn: conn}) do
    msg = :scramerl.client_first_message(username)
    {:ok, conn, _request_ref} = Mint.HTTP.request(conn, "GET", "/auth", [], msg)
    {:noreply, %{state | conn: conn, auth_stage: :client_first}}
  end

  @impl true
  def handle_cast(data = %{message: 'server-first-message'},
	state = %{password: password}) do

    {:ok, salt} = Map.fetch(data, :salt)
    {:ok, ic} = Map.fetch(data, :"iteration-count")
    {:ok, nonce} = Map.fetch(data, :nonce)

    normalized_pw = :stringprep.prepare(password)
    salted_pw = :scramerl_lib.hi(normalized_pw, salt, ic)
    
    {:noreply, state}
  end

  @impl true
  def handle_info({transport, socket, http_response}, state = %{conn: conn, socket: socket}) do
    {:ok, conn, response} = Mint.HTTP.stream(conn, {transport, socket, http_response})
    {:data, _ref, data}  = :lists.keyfind(:data, 1, response)
    
    scram_data = :scramerl_lib.parse(data)
    Logger.debug("Auth client received data: #{inspect(scram_data, pretty: true)}")
    GenServer.cast(__MODULE__, scram_data)
    {:noreply, %{state | conn: conn}}
  end

  @impl true
  def handle_info({:tcp_closed, socket}, state = %{socket: socket}) do
    Logger.debug("TCP closed...")
    {:noreply, %{state | conn: :nil, socket: :nil}}
  end

  ### Utility functions ###
  
  defp charlist(text), do: text |> to_charlist()
  
end

# username:
# test_user

# password:
# test_pw (not in db)

# pass_details:
# ==SCRAM==,jv1SCgihx+Q2yj6PggxUZPbmfp4=,r+T1xjRnDwpUPoC/EwOXA+Jjt2Y=,iCgKQkjMSgfZgjh06UMZzg==,4096
