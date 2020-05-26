defmodule Client.AdminServer do
  @moduledoc """
  GenServer for administrative actions, such as setting privileges.
  Not all users have access to these functionalities.
  """
  require Logger
  alias Client.RestLib, as: Rest

  use GenServer

  defstruct(
    username: :nil,
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

  @doc """
  Interrogates own privilege from database through server.
  """
  def get_privilege() do
    GenServer.call(__MODULE__, :get_privilege)
  end

  @doc """
  Interrogates all users with respective privileges from database through server.
  Gets rejected if user is not admin.
  """
  def get_all_privilege() do
    GenServer.call(__MODULE__, :get_all_privilege)
  end
  
  @doc """
  Changing the privilege of another user.
  """
  def set_privilege(username, privilege) do
    GenServer.call(__MODULE__, {:set_privilege, string(username), string(privilege)})
  end

  
  ### Server callbacks ###
  @impl true
  def init(options) do
    state = parse_options(options)
    {:ok, state}
  end

  @impl true
  def handle_call(:get_privilege, from, state) do
    body = %{get_all: false}
    Rest.send_http_id(body, from, "GET", "/users/privileges", state)
  end

  @impl true
  def handle_call(:get_all_privilege, from, state) do
    body = %{get_all: true}
    Rest.send_http_id(body, from, "GET", "/users/privileges", state)
  end
  
  @impl true
  def handle_call({:set_privilege, self, _priv}, _from, state = %{username: self}) do
    result = "ERROR_set_self_privilege"
    {:reply, result, state}
  end
  
  @impl true
  def handle_call({:set_privilege, username, privilege}, from, state) do
    body = %{target: username, privilege: privilege}
    Rest.send_http_id(body, from, "PUT", "/users/privileges", state)
  end


  @impl true
  def handle_info(http_info = {_transport, _socket, _http_response}, state) do
    Rest.receive_http(http_info, state)
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
  defp parse_options([{:username, username} | remaining_opts], state) do
    parse_options(remaining_opts, %{state | username: username})
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
  
  defp string(text), do: text |> to_string()
  
end
