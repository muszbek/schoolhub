defmodule Schoolhub.Router do
  @moduledoc """
  HTTP router through Plug.
  """
  require Logger

  use Plug.Router

  @http_response_timeout 1_000

  plug :match
  plug :dispatch

  get "/auth" do
    {:ok, body, conn} = Plug.Conn.read_body(conn)
    Logger.debug("Received GET request on /auth with body: #{inspect(body)}")

    Schoolhub.AuthServer.authenticate(body)
    http_respond(conn)
  end

  match _ do
    send_resp(conn, 404, "Oops!")
  end

  
  defp http_respond(conn) do
    receive do
      {:reply, response_body} ->
	send_resp(conn, 200, response_body)
    after
      @http_response_timeout ->
	raise "http_timeout"
    end	
  end
  
end
