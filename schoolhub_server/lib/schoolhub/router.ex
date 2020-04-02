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
    response_body = GenServer.call(Schoolhub.Auth, {:auth, body})
    Logger.debug(inspect(response_body, pretty: true))
    send_resp(conn, 200, response_body)
  end

  match _ do
    send_resp(conn, 404, "Oops!")
  end
  
end
