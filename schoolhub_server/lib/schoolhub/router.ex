defmodule Schoolhub.Router do
  @moduledoc """
  HTTP router through Plug.
  """
  require Logger

  use Plug.Router

  plug :match
  plug :dispatch

  get "/auth" do
    Logger.debug("Received GET request on /auth")
    Logger.debug(inspect(conn, pretty: true))
    conn
  end

  match _ do
    send_resp(conn, 404, "Oops!")
  end
  
end
