defmodule Client.ChatArchiveServer do
  @moduledoc """
  GenServer handling chat archive through a REST API.
  No XMPP connection.
  """
  require Logger

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

  @default_server_limit 10

  ### API functions ###

  @doc false
  def start_link(options) do
    GenServer.start_link(__MODULE__, options, name: __MODULE__)
  end

  @doc false
  def get_server_archive(partner) do
    get_server_archive(partner, @default_server_limit)
  end
  def get_server_archive(partner, limit) do
    GenServer.call(__MODULE__, {:get_server_archive, partner, limit})
  end


  ### Server callbacks ###
  @impl true
  def init(options) do
    {:ok, parse_options(options)}
  end

  
  @impl true
  def handle_call({:get_server_archive, partner, limit}, from, state = %{username: self,
								  scheme: scheme,
								  ip: ip,
								  port: port}) do

    msg = Jason.encode!(%{self: self, partner: partner, limit: limit})
    {:ok, conn} = Mint.HTTP.connect(scheme, ip, port)
    {:ok, conn, _request_ref} = Mint.HTTP.request(conn, "GET", "/get_mam", [], msg)
    
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
    {:data, _ref, data_json}  = :lists.keyfind(:data, 1, response)
    data = Jason.decode!(data_json)
    
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
  
end
