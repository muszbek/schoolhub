defmodule Client.ChatArchiveServer do
  @moduledoc """
  GenServer handling chat archive through a REST API.
  No XMPP connection.
  """
  require Logger
  alias Client.RestLib, as: Rest

  use GenServer

  defstruct(
    username: :nil,
    scheme: :http,
    ip: "localhost",
    port: 8080,
    server_opts: [],
    conn: :nil,
    socket: :nil,
    reg_caller: :nil,
    conv_table: :nil
  )

  @default_server_limit 10
  @ets_name :conversations

  ### API functions ###

  @doc false
  def start_link(options) do
    GenServer.start_link(__MODULE__, options, name: __MODULE__)
  end

  @doc """
  Deletes all the local (client-side) in-memory message archives.
  They can be again interrogated dynamically from the server.
  """
  def refresh_cache() do
    GenServer.call(__MODULE__, :refresh_cache)
  end

  @doc """
  Adds new chat message to local cache archive.
  If archive corresponding to that user is empty, interrogates server through REST.
  """
  def handle_chat({partner, msg = [_direction, _body]}) do
    table = @ets_name
    case :ets.lookup(table, partner) do
      [] ->
	messages = get_server_archive(partner) ++ [msg]
	count = get_initial_message_count(messages)
	true = :ets.insert(table, {partner, messages, count})
      [{partner, messages, :all}] ->
	true = :ets.insert(table, {partner, messages ++ [msg], :all})
      [{partner, messages, count}] ->
	true = :ets.insert(table, {partner, messages ++ [msg], count+1})
    end
    :ok
  end

  @doc """
  Returns locally stored archive corresponding to partner.
  If archive is empty, queries the server.
  """
  def get_archive(partner) do
    case get_local_archive(partner) do
      [] ->
	case get_server_archive(partner) do
	  [] -> []
	  messages ->
	    table = @ets_name
	    count = get_initial_message_count(messages)
	    true = :ets.insert(table, {partner, messages, count})
	    messages
	end
      
      messages -> messages
    end
  end

  @doc """
  Returns locally stored archive corresponding to partner.
  """
  def get_local_archive(partner) do
    table = @ets_name
    case :ets.lookup(table, partner) do
      [] -> []
      [{^partner, messages, _count}] -> messages
    end
  end

  @doc """
  Loads more messages from the server, increasing the interrogation limit.
  If all the messages are already stored locally, no more queries to the server.
  """
  def get_more_archive(partner) do
    table = @ets_name
    case :ets.lookup(table, partner) do
      [] -> []
      [{^partner, messages, :all}] -> messages
      [{^partner, _messages, count}] ->
	limit = count + @default_server_limit
	new_messages = get_server_archive(partner, limit)
	new_count = case length(new_messages) do
		      number when number < limit -> :all
		      ^limit -> limit
		    end
	
	true = :ets.insert(table, {partner, new_messages, new_count})
	new_messages
    end
  end
    
  @doc """
  Fetches mam from server through REST api.
  """
  def get_server_archive(partner) do
    get_server_archive(partner, @default_server_limit)
  end
  def get_server_archive(partner, limit) do
    GenServer.call(__MODULE__, {:get_server_archive, partner, limit})
  end


  ### Server callbacks ###
  @impl true
  def init(options) do
    state = parse_options(options)
    conversations = :ets.new(@ets_name, [:set, :public, :named_table])
    {:ok, %{state | conv_table: conversations}}
  end

  
  @impl true
  def handle_call({:get_server_archive, partner, limit}, from, state) do
    body = %{partner: partner, limit: limit}
    Rest.send_http_id(body, from, "GET", "/get_mam", state)
  end

  @impl true
  def handle_call(:refresh_cache, _from, state = %{conv_table: table}) do
    :true = :ets.delete(table)
    new_table = :ets.new(@ets_name, [:set, :public, :named_table])
    {:reply, :ok, %{state | conv_table: new_table}}
  end


  @impl true
  def handle_cast({:new_chat_msg, {partner, msg = [_direction, _body]}},
	state = %{conv_table: table}) do
    
    case :ets.lookup(table, partner) do
      [] ->
	messages = get_server_archive(partner) ++ [msg]
	count = length(messages)
	:ets.insert(table, {partner, messages, count})
      [{partner, messages, count}] ->
	:ets.insert(table, {partner, messages ++ [msg], count+1})
    end
    {:noreply, state}
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
  defp parse_options([{:opts, server_opts} | remaining_opts], state) do
    parse_options(remaining_opts, %{state | server_opts: server_opts})
  end
  defp parse_options([{_key, _value} | remaining_opts] ,state) do
    parse_options(remaining_opts, state)
  end


  defp get_initial_message_count(messages) do
    case length(messages) do
      number when number < @default_server_limit -> :all
      number -> number
    end
  end
  
end
