defmodule Client.LoginServer do
  @moduledoc """
  GenServer sending out requests to authenticate and create session
  or registering new user.
  """
  require Logger
  alias Client.RestLib, as: Rest

  use GenServer

  @supervisor Client.Supervisor

  defstruct(
    scheme: :http,
    ip: "localhost",
    port: 8080,
    server_opts: [],
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
  def handle_call({:reg, username, password}, from, state) do
    body = %{username: username, password: password}
    Rest.send_http(body, from, "POST", "/users", state)
  end

  @impl true
  def handle_call({:remove, username}, from, state) do
    body = %{username: username}
    Rest.send_http(body, from, "DELETE", "/users", state)
  end


  @impl true
  def handle_info(http_info = {_transport, _socket, _http_response}, state) do
    Rest.receive_http(http_info, state)
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
  
end
