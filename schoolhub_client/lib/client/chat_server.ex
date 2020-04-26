defmodule Client.ChatServer do
  @moduledoc """
  GenServer for XMPP communication, implementing the Romeo library.
  """
  require Logger

  use GenServer

  defstruct(
    xmpp_api: Romeo,
    xmpp_hostname: "localhost",
    conn: :nil,
    readiness: :not_connected
  )

  ### API functions ###
  
  @doc false
  def start_link(args) do
    GenServer.start_link(__MODULE__, args, name: __MODULE__)
  end

  @doc false
  def add_roster(username) do
    GenServer.call(__MODULE__, {:add_roster, username, []})
  end
  def add_roster(username, hostname) do
    GenServer.call(__MODULE__, {:add_roster, username, hostname})
  end

  @doc false
  def remove_roster(username) do
    GenServer.call(__MODULE__, {:remove_roster, username, []})
  end
  def remove_roster(username, hostname) do
    GenServer.call(__MODULE__, {:remove_roster, username, hostname})
  end

  @doc false
  def get_roster() do
    GenServer.call(__MODULE__, :get_roster)
  end

  
  ### Server callbacks ###
  @impl true
  def init({username, password}) do
    state = %__MODULE__{}

    host = host()
    xmpp_api = xmpp_backend()
    xmpp_conn = Module.concat(xmpp_api, Connection)
    xmpp_stanza = Module.concat(xmpp_api, Stanza)
    
    creds = get_conn_opts(username, password, host)
    {:ok, conn} = xmpp_conn.start_link(creds)
    :ok = xmpp_conn.send(conn, xmpp_stanza.presence)
    
    {:ok, %{state | xmpp_api: xmpp_api, xmpp_hostname: host, conn: conn}}
  end

  @impl true
  def handle_call({:add_roster, username, []}, _from,
	state = %{xmpp_api: xmpp_api,
		  xmpp_hostname: host,
		  conn: conn,
		  readiness: :ready}) do
    
    result = add_roster(conn, xmpp_api, {username, host})
    {:reply, result, state}
  end

  @impl true
  def handle_call({:add_roster, username, hostname}, _from,
	state = %{xmpp_api: xmpp_api,
		  conn: conn,
		  readiness: :ready}) do
    
    result = add_roster(conn, xmpp_api, {username, hostname})
    {:reply, result, state}
  end

  @impl true
  def handle_call({:remove_roster, username, []}, _from,
	state = %{xmpp_api: xmpp_api,
		  xmpp_hostname: host,
		  conn: conn,
		  readiness: :ready}) do
    
    result = remove_roster(conn, xmpp_api, {username, host})
    {:reply, result, state}
  end

  @impl true
  def handle_call({:remove_roster, username, hostname}, _from,
	state = %{xmpp_api: xmpp_api,
		  conn: conn,
		  readiness: :ready}) do
    
    result = remove_roster(conn, xmpp_api, {username, hostname})
    {:reply, result, state}
  end

  @impl true
  def handle_call(:get_roster, _from,
	state = %{xmpp_api: xmpp_api,
		  conn: conn,
		  readiness: :ready}) do
    
    xmpp_roster = Module.concat(xmpp_api, Roster)
    items = xmpp_roster.items(conn)
    {:reply, items, state}
  end
  

  @impl true
  def handle_info({:resource_bound, _res}, state = %{readiness: :not_connected}) do
    {:noreply, %{state | readiness: :bound}}
  end

  @impl true
  def handle_info(:connection_ready, state = %{readiness: :bound}) do
    {:noreply, %{state | readiness: :ready}}
  end
  
  @impl true
  def handle_info(info, state) do
    Logger.warn(inspect(info))
    {:noreply, state}
  end


  ### Utility functions ###

  defp string(text), do: text |> to_string()

  defp host() do
    Application.get_env(:schoolhub_client, :mongooseim_hostname, "localhost")
  end
  
  defp get_conn_opts(username, password, host) do
    [jid: string(username) <> "@" <> host, password: string(password)]
  end

  defp xmpp_backend() do
    Application.get_env(:schoolhub_client, :xmpp_backend, Romeo)
  end


  defp add_roster(conn, xmpp_api, {username, hostname}) do
    xmpp_roster = Module.concat(xmpp_api, Roster)
    jid = string(username) <> "@" <> string(hostname)

    :ok = xmpp_roster.add(conn, jid)
  end

  defp remove_roster(conn, xmpp_api, {username, hostname}) do
    xmpp_roster = Module.concat(xmpp_api, Roster)
    jid = string(username) <> "@" <> string(hostname)

    :ok = xmpp_roster.remove(conn, jid)
  end

end
