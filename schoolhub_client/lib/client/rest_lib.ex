defmodule Client.RestLib do
  @moduledoc """
  A library module to separate the frequently reoccuring HTTP routing 
  in the client code.
  """

  @doc """
  Intended to call from a handle_call callback.
  Does not return immediately, but waits for response info.
  """
  def send_http(dataMap, from, method, path, state = %{scheme: scheme,
						      ip: ip,
						      port: port}) do
    msg = Jason.encode!(dataMap)
    {:ok, conn} = Mint.HTTP.connect(scheme, ip, port)
    {:ok, conn, _request_ref} = Mint.HTTP.request(conn, method, path, [], msg)
    
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
  def receive_http({transport, socket, http_response}, state = %{conn: conn,
								socket: socket,
								reg_caller: from}) do
    
    {:ok, _conn, response} = Mint.HTTP.stream(conn, {transport, socket, http_response})
    {:status, _ref, status_code} = :lists.keyfind(:status, 1, response)
    {:data, _ref, data_json}  = :lists.keyfind(:data, 1, response)

    data = case status_code do
	     400 -> "ERROR_404"
	     _ -> Jason.decode!(data_json)
	   end
    
    GenServer.reply(from, data)
    {:noreply, state}
  end
  
end
