defmodule Client.RestLib do
  @moduledoc """
  A library module to separate the frequently reoccuring HTTP routing 
  in the client code.
  """
  require Logger

  @doc """
  Intended to call from a handle_call callback.
  Does not return immediately, but waits for response info.
  """
  def send_http(dataMap, from, method, path, state = %{scheme: scheme,
						       ip: ip,
						       port: port,
						       server_opts: opts}) do
    msg = Jason.encode!(dataMap)
    {:ok, conn} = Mint.HTTP1.connect(scheme, ip, port, opts)
    {:ok, conn, _request_ref} = Mint.HTTP1.request(conn, method, path, [], msg)
    
    {:noreply, %{state |
		 conn: conn,
		 socket: conn.socket,
		 reg_caller: from}}
  end

  @doc """
  Puts self user id into the request body.
  """
  def send_http_id(dataMap, from, method, path, state = %{username: self}) do
    map_with_id = Map.merge(%{self: self}, dataMap)
    send_http(map_with_id, from, method, path, state)
  end

  @doc """
  Handling the http response and replying to the original call.
  """
  def receive_http(message = {_transport, socket, _http_response}, state = %{conn: conn,
									     socket: socket,
									     reg_caller: from}) do
    
    {:ok, _conn, response} = Mint.HTTP1.stream(conn, message)
    {:status, _ref, status_code} = :lists.keyfind(:status, 1, response)
    {:data, _ref, data_json}  = :lists.keyfind(:data, 1, response)

    data = case status_code do
	     400 -> "400_bad_request"
	     404 -> "404_not_found"
	     503 ->
	       Logger.warn("503 Service Unavailable")
	       "503_service_unavailable"
	     _ -> Jason.decode!(data_json)
	   end
    
    GenServer.reply(from, data)
    {:noreply, state}
  end
  
  def receive_http(_message = {_transport, _socket, _http_response},
	state = %{socket: _other_socket}) do

    Logger.warn("HTTP response to timed out request received")
    {:noreply, state}
  end

  @doc """
  Handling the http response gen_server info tcp closed.
  """
  def tcp_closed(socket, state = %{socket: socket}) do
    Logger.debug("TCP closed for socket #{inspect(socket)}")
    {:noreply, %{state |
		 conn: :nil,
		 socket: :nil}}
  end
  def tcp_closed(socket, state = %{socket: _other_socket}) do
    ## This message does not affect the current registration session.
    Logger.debug("TCP closed for socket #{inspect(socket)}")
    {:noreply, state}
  end

  @doc """
  Handling the http response gen_server info tcp closed.
  """
  def ssl_closed(socket, state = %{socket: socket}) do
    Logger.debug("SSL closed for socket #{inspect(socket)}")
    {:noreply, %{state |
		 conn: :nil,
		 socket: :nil}}
  end
  def ssl_closed(socket, state = %{socket: _other_socket}) do
    ## This message does not affect the current registration session.
    Logger.debug("SSL closed for socket #{inspect(socket)}")
    {:noreply, state}
  end
  
end
