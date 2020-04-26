defmodule Client.LoginServer do
  @moduledoc """
  GenServer sending out requests to authenticate and create session
  or registering new user.
  """
  require Logger

  use GenServer

  @supervisor Client.Supervisor

  defstruct(
    scheme: :http,
    ip: "localhost",
    port: 8080,
    conn: :nil,
    socket: :nil,
    reg_caller: :nil
  )

  ### API functions ###

  @doc false
  def start_link(options) do
    GenServer.start_link(__MODULE__, options, name: __MODULE__)
  end

  @doc false
  def start_session(username, password) do
    auth_result = Client.Auth.auth(username, password)
    
    if auth_result == :authenticated do
      Logger.debug("Starting session...")
      @supervisor.sup_start_session(username, password)
    else
      auth_result
    end
  end

  @doc false
  def end_session() do
    Logger.debug("Ending session...")
    @supervisor.sup_stop_session()
  end

  @doc false
  def reg_user(username, password) do
    _reg_result = GenServer.call(__MODULE__, {:reg, username, password})
  end

  @doc false
  def remove_user(username, password) do
    auth_result = Client.Auth.auth(username, password)
    
    if auth_result == :authenticated do
      Logger.debug("Removing user...")
      _remove_result = GenServer.call(__MODULE__, {:remove, username})
    else
      auth_result
    end
  end


  ### Server callbacks ###
  @impl true
  def init(options) do
    {:ok, parse_options(options)}
  end

  @impl true
  def handle_call({:reg, username, password}, from, state = %{scheme: scheme,
							      ip: ip,
							      port: port}) do
    
    msg = Jason.encode!(%{username: username, password: password})
    {:ok, conn} = Mint.HTTP.connect(scheme, ip, port)
    {:ok, conn, _request_ref} = Mint.HTTP.request(conn, "GET", "/reg_user", [], msg)
    
    {:noreply, %{state |
		 conn: conn,
		 socket: conn.socket,
		 reg_caller: from}}
  end

  @impl true
  def handle_call({:remove, username}, from, state = %{scheme: scheme,
						       ip: ip,
						       port: port}) do
    
    msg = username |> to_string()
    {:ok, conn} = Mint.HTTP.connect(scheme, ip, port)
    {:ok, conn, _request_ref} = Mint.HTTP.request(conn, "GET", "/remove_user", [], msg)
    
    {:noreply, %{state |
		 conn: conn,
		 socket: conn.socket,
		 reg_caller: from}}
  end

  @impl true
  def handle_info({transport, socket, http_response}, state = %{conn: conn,
								socket: socket,
								reg_caller: from}) do

    {:ok, _conn, response} = Mint.HTTP.stream(conn, {transport, socket, http_response})
    {:data, _ref, data}  = :lists.keyfind(:data, 1, response)

    GenServer.reply(from, data)
    {:noreply, state}
  end

  @impl true
  def handle_info({:tcp_closed, socket}, state = %{socket: socket}) do
    Logger.debug("TCP closed for socket #{inspect(socket)}")
    {:noreply, %{state |
		 conn: :nil,
		 socket: :nil}}
  end

  @impl true
  def handle_info({:tcp_closed, socket}, state = %{socket: _other_socket}) do
    ## This message does not affect the current registration session.
    Logger.debug("TCP closed for socket #{inspect(socket)}")
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
  defp parse_options([{:scheme, scheme} | remaining_opts], state) do
    parse_options(remaining_opts, %{state | scheme: scheme})
  end
  defp parse_options([{:ip, ip} | remaining_opts], state) do
    parse_options(remaining_opts, %{state | ip: ip})
  end
  defp parse_options([{:port, port} | remaining_opts], state) do
    parse_options(remaining_opts, %{state | port: port})
  end
  defp parse_options([{_key, _value} | remaining_opts] ,state) do
    parse_options(remaining_opts, state)
  end
  
end
