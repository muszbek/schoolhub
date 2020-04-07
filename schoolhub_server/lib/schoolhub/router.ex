defmodule Schoolhub.Router do
  @moduledoc """
  HTTP router through Plug.
  """
  require Logger

  use Plug.Router

  plug :match
  plug :dispatch

  get "/auth" do
    {:ok, body, conn} = Plug.Conn.read_body(conn)
    Logger.debug("Received GET request on /auth with body: #{inspect(body)}")
    msg = handle_html_msg(body)
    response_body = GenServer.call(Schoolhub.AuthStateMachine, {:auth, msg})
    send_resp(conn, 200, response_body)
  end

  match _ do
    send_resp(conn, 404, "Oops!")
  end

  
  defp handle_html_msg(data) do
    scram_data = :scramerl_lib.parse(data)
    Logger.debug("Auth server received data: #{inspect(scram_data, pretty: true)}")
    scram_data
  end
  
end
