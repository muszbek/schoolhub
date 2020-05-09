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
    Logger.debug("Received GET request on /reg_user with body: #{inspect(body)}")
    
    %{"username" => username, "password" => password} = Jason.decode!(body)
    result = Schoolhub.RegServer.register_user(username, password)

    response_body = result |> encode_response()
    send_resp(conn, 200, response_body)
  end

  get "/remove_user" do
    {:ok, body, conn} = Plug.Conn.read_body(conn)
    Logger.debug("Received GET request on /remove_user with body: #{inspect(body)}")
    
    username = body
    result = Schoolhub.RegServer.remove_user(username)

    response_body = result |> encode_response()
    send_resp(conn, 200, response_body)
  end

  get "/get_mam" do
    {:ok, body, conn} = Plug.Conn.read_body(conn)
    Logger.debug("Received GET request on /get_mam with body: #{inspect(body)}")

    %{"self" => self, "partner" => partner, "limit" => limit} = Jason.decode!(body)
    result = Schoolhub.ChatServer.get_archive(self, partner, limit)

    {:ok, response_body} = Jason.encode(result)
    send_resp(conn, 200, response_body)
  end

  get "/get_privilege" do
    {:ok, body, conn} = Plug.Conn.read_body(conn)
    Logger.debug("Received GET request on /get_privilege with body: #{inspect(body)}")

    %{"user" => self, "get_all" => get_all} = Jason.decode!(body)
    result = case get_all do
	       false -> Schoolhub.RegServer.get_user_privilege(self)
	       true -> Schoolhub.RegServer.get_all_privilege(self)
	     end

    response_body = result |> encode_response()
    send_resp(conn, 200, response_body)
  end

  get "/set_privilege" do
    {:ok, body, conn} = Plug.Conn.read_body(conn)
    Logger.debug("Received GET request on /set_privilege with body: #{inspect(body)}")

    %{"self" => self, "target" => target, "privilege" => priv} = Jason.decode!(body)
    result = Schoolhub.RegServer.set_user_privilege(self, target, priv)

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
    result = case response do
	       {:ok, _reason} -> "ok"
	       {:error, reason} -> "ERROR_" <> Atom.to_string(reason)
	       :ok -> "ok"
	       other -> other
	     end
    
    result |> Jason.encode!()
  end
  
end
