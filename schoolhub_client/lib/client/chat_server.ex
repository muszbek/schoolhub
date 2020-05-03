defmodule Client.ChatServer do
  @moduledoc """
  GenServer for XMPP communication, implementing the Romeo library.
  """
  require Logger

  use GenServer
  use Romeo.XML
  ## Romeo.Stanza alias Stanza implied in Romeo.XML
  ## That module is just data structures and conversions, independent of back-end

  defstruct(
    xmpp_api: Romeo,
    xmpp_hostname: "localhost",
    username: :nil,
    conn: :nil,
    readiness: :not_connected
  )

  ### API functions ###
  
  @doc false
  def start_link(args) do
    GenServer.start_link(__MODULE__, args, name: __MODULE__)
  end

  ## Roster API
  @doc false
  def add_roster(username, hostname \\ "", name \\ "") do
    GenServer.call(__MODULE__, {:add_roster, username, hostname, name})
  end

  @doc false
  def remove_roster(username, hostname \\ "") do
    GenServer.call(__MODULE__, {:remove_roster, username, hostname})
  end

  @doc false
  def get_roster() do
    GenServer.call(__MODULE__, :get_roster)
  end

  ## Chat API
  @doc false
  def chat(username, msg, hostname \\ "") do
    GenServer.call(__MODULE__, {:chat, username, hostname, msg})
  end

  ## Inbox API
  @doc false
  def get_inbox_form() do
    GenServer.call(__MODULE__, :get_inbox_form)
  end

  @doc false
  def read_inbox() do
    read_inbox(1)
  end
  def read_inbox(days_before) do
    GenServer.call(__MODULE__, {:read_inbox, days_before})
  end
  
  
  ### Server callbacks ###
  @impl true
  def init({username, password}) do
    state = %__MODULE__{}

    host = host()
    xmpp_api = xmpp_backend()
    xmpp_conn = Module.concat(xmpp_api, Connection)
    
    creds = get_conn_opts(username, password, host)
    {:ok, conn} = xmpp_conn.start_link(creds)
    :ok = xmpp_conn.send(conn, Stanza.presence)
    
    {:ok, %{state | xmpp_api: xmpp_api, xmpp_hostname: host, username: username, conn: conn}}
  end

  @impl true
  def handle_call({:add_roster, username, hostname, name}, _from,
	state = %{xmpp_api: xmpp_api,
		  xmpp_hostname: host,
		  conn: conn,
		  readiness: :ready}) do
    
    hostname = case hostname do
		 "" -> host
		 name -> name
	       end

    name = case name do
	     "" -> username
	     name -> name
	   end
    
    result = do_add_roster(conn, xmpp_api, {username, hostname, name})
    {:reply, result, state}
  end

  @impl true
  def handle_call({:remove_roster, username, hostname}, _from,
	state = %{xmpp_api: xmpp_api,
		  xmpp_hostname: host,
		  conn: conn,
		  readiness: :ready}) do

    hostname = case hostname do
		 "" -> host
		 name -> name
	       end
    
    result = do_remove_roster(conn, xmpp_api, {username, hostname})
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
  def handle_call({:chat, username, hostname, msg}, _from,
	state = %{xmpp_api: xmpp_api,
		  xmpp_hostname: host,
		  conn: conn,
		  readiness: :ready}) do

    hostname = case hostname do
		 "" -> host
		 name -> name
	       end

    result = do_chat(conn, xmpp_api, {username, hostname}, msg)
    {:reply, result, state}
  end

  @impl true
  def handle_call(:get_inbox_form, _from,
	state = %{xmpp_api: xmpp_api,
		  conn: conn,
		  readiness: :ready}) do

    xmpp_conn = Module.concat(xmpp_api, Connection)
    stanza = Stanza.iq("get", xmlel(name: "query", attrs: [{"xmlns", ns_inbox()}]))
    result = xmpp_conn.send(conn, stanza)
    {:reply, result, state}
  end

  @impl true
  def handle_call({:read_inbox, days_before}, _from,
	  state = %{xmpp_api: xmpp_api,
		  conn: conn,
		  readiness: :ready}) do

    seconds_before = 3600 * 24 * days_before
    today = DateTime.utc_now()
    yesterday = DateTime.add(today, -seconds_before, :second)
    
    stanza = read_inbox_stanza(isodate(yesterday), isodate(today))

    xmpp_conn = Module.concat(xmpp_api, Connection)
    result = xmpp_conn.send(conn, stanza)
    {:reply, result, state}
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
  def handle_info({:stanza, %Romeo.Stanza.Presence{from: %Romeo.JID{user: username},
						   to: %Romeo.JID{user: username}}},
	state = %{username: username}) do

    {:noreply, state}
  end
  
  @impl true
  def handle_info({:stanza, %Romeo.Stanza.Message{type: "chat",
						  body: msg,
						  from: %Romeo.JID{user: sender},
						  to: %Romeo.JID{user: username}}},
	state = %{username: username}) do

    chat_received = {sender, ["I", msg]}
    add_chat_to_archive(chat_received)
    Logger.info(sender <> " says: " <> msg)
    {:noreply, state}
  end

  @impl true
  def handle_info({:stanza,
		   %Romeo.Stanza.Message{type: "normal",
					 body: "",
					 from: %Romeo.JID{user: username},
					 xml: {:xmlel, "message", _, [
						  {:xmlel, "result", _, [
						      {:xmlel, "forwarded", _, [
							  {:xmlel, "delay", [_, {"stamp",
										 timestamp}],
							   []}, message]}]}]}}},
	state = %{username: username}) do

    {:xmlel, "message", [{"from", sender_full},
			 {"to", _jid},
			 {"type", "chat"}, _, _],
     [{:xmlel, "body", [], msg_body}]} = message

    case msg_body do
      [] -> :ok
      [xmlcdata: msg_body] ->
	[sender | _] = String.split(sender_full, "@")
	Logger.info("Last message from " <> sender <> " at " <> timestamp <> ": " <> msg_body)
    end
    {:noreply, state}
  end

  @impl true
  def handle_info({:stanza, %Romeo.Stanza.IQ{from: %Romeo.JID{user: username},
					     to: %Romeo.JID{user: username},
					     type: "result",
					     xml: {:xmlel, "iq", _, [
						      {:xmlel, "fin", _, content}]}}},
	state = %{username: username}) do

    [{:xmlel, "active-conversations", [], [xmlcdata: active_conv]},
     {:xmlel, "count", [], [xmlcdata: count]},
     {:xmlel, "unread-messages", [], [xmlcdata: unread_msg]}] = content
    
    Logger.info("Total conversations: " <> count <>
      "\nActive conversations: " <> active_conv <>
      "\nUnread messages: " <> unread_msg)
    
    {:noreply, state}
  end
  
  @impl true
  def handle_info(info, state) do
    Logger.warn(inspect(info, pretty: true))
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


  defp do_add_roster(conn, xmpp_api, {username, hostname, name}) do
    xmpp_roster = Module.concat(xmpp_api, Roster)
    jid = string(username) <> "@" <> string(hostname)

    :ok = xmpp_roster.add(conn, jid, name)
  end

  defp do_remove_roster(conn, xmpp_api, {username, hostname}) do
    xmpp_roster = Module.concat(xmpp_api, Roster)
    jid = string(username) <> "@" <> string(hostname)

    :ok = xmpp_roster.remove(conn, jid)
  end

  defp do_chat(conn, xmpp_api, {username, hostname}, msg) do
    xmpp_conn = Module.concat(xmpp_api, Connection)
    jid = string(username) <> "@" <> string(hostname)

    :ok = xmpp_conn.send(conn, Stanza.chat(jid, msg))
    add_chat_to_archive({username, ["O", msg]})
  end

  defp add_chat_to_archive(msg = {_sender, [_direction, _body]}) do
    Client.ChatArchiveServer.handle_chat(msg)
  end

  defp isodate(date), do: date |> DateTime.to_iso8601()

  defp ns_inbox, do: "erlang-solutions.com:xmpp:inbox:0"
  
  defp read_inbox_stanza(start_time, end_time, order \\ "asc", hidden_read \\ "false") do

    field_type = inbox_stanza_field("hidden", "FORM_TYPE", ns_inbox())
    field_start = inbox_stanza_field("text-single", "start", start_time)
    field_end = inbox_stanza_field("text-single", "end", end_time)
    field_order = inbox_stanza_field("list-single", "order", order)
    field_hidden = inbox_stanza_field("text-single", "hidden_read", hidden_read)
    
    x = xmlel(name: "x", attrs: [{"xmlns", ns_data_forms()},
				 {"type", "form"}],
      children: [field_type, field_start, field_end, field_order, field_hidden])
    
    Stanza.iq("set", xmlel(name: "inbox",
	  attrs: [{"xmlns", ns_inbox()},
		  {"queryid", Stanza.id()}], children: [x]))
  end

  defp inbox_stanza_field(type, var, value) do
    xmlel(name: "field", attrs: [{"type", type}, {"var", var}],
      children: [xmlel(name: "value", children: [Stanza.cdata(value)])])
  end

end
