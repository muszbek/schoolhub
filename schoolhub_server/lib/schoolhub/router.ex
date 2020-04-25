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

  get "/reg_user" do
    {:ok, body, conn} = Plug.Conn.read_body(conn)
    Logger.debug("Received GET request on /auth with body: #{inspect(body)}")
    
    %{"username" => username, "password" => password} = Jason.decode!(body)
    result = Schoolhub.RegServer.register_user(username, password)

    response_body = result |> encode_response()
    send_resp(conn, 200, response_body)
  end

  get "/remove_user" do
    {:ok, body, conn} = Plug.Conn.read_body(conn)
    Logger.debug("Received GET request on /auth with body: #{inspect(body)}")
    
    username = body
    result = Schoolhub.RegServer.remove_user(username)

    response_body = result |> encode_response()
    send_resp(conn, 200, response_body)
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

  defp encode_response(response) do
    case response do
      {:ok, _reason} -> "ok"
      {:error, reason} -> "ERROR_" <> Atom.to_string(reason)
      :ok -> "ok"
      other -> other |> inspect() |> to_string() |> Jason.encode!()
    end
  end
  
end
