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
    body = read_body(conn, "GET", "/auth")

    Schoolhub.AuthServer.authenticate(body)
    http_respond(conn)
  end

  put "/reg_user" do
    body = read_body(conn, "PUT", "/reg_user")
    
    %{"username" => username, "password" => password} = Jason.decode!(body)
    result = Schoolhub.RegServer.register_user(username, password)

    {code, response_body} = result |> encode_response()
    send_resp(conn, code, response_body)
  end

  put "/remove_user" do
    body = read_body(conn, "PUT", "/remove_user")
    
    %{"username" => username} = Jason.decode!(body)
    result = Schoolhub.RegServer.remove_user(username)

    {code, response_body} = result |> encode_response()
    send_resp(conn, code, response_body)
  end
  
  get "/get_privilege" do
    body = read_body(conn, "GET", "/get_privilege")

    %{"get_all" => get_all, "self" => self} = Jason.decode!(body)
    result = case get_all do
	       false -> Schoolhub.RegServer.get_user_privilege(self)
	       true -> Schoolhub.RegServer.get_all_privilege(self)
	     end

    {code, response_body} = result |> encode_response()
    send_resp(conn, code, response_body)
  end

  put "/set_privilege" do
    body = read_body(conn, "PUT", "/set_privilege")

    %{"self" => self, "target" => target, "privilege" => priv} = Jason.decode!(body)
    result = Schoolhub.RegServer.set_user_privilege(self, target, priv)

    {code, response_body} = result |> encode_response()
    send_resp(conn, code, response_body)
  end

  
  get "/get_mam" do
    body = read_body(conn, "GET", "/get_mam")

    %{"self" => self, "partner" => partner, "limit" => limit} = Jason.decode!(body)
    result = Schoolhub.ChatServer.get_archive(self, partner, limit)

    {code, response_body} = result |> encode_response()
    send_resp(conn, code, response_body)
  end


  put "/create_course" do
    body = read_body(conn, "PUT", "/create_course")

    %{"self" => self, "course_name" => course_name} = Jason.decode!(body)
    result = Schoolhub.CourseServer.create_course(self, course_name)

    {code, response_body} = result |> encode_response()
    send_resp(conn, code, response_body)
  end

  put "/remove_course" do
    body = read_body(conn, "PUT", "/remove_course")

    %{"self" => self, "course_name" => course_name} = Jason.decode!(body)
    result = Schoolhub.CourseServer.remove_course(self, course_name)

    {code, response_body} = result |> encode_response()
    send_resp(conn, code, response_body)
  end

  get "/get_affiliation" do
    body = read_body(conn, "GET", "/get_affiliation")

    %{"self" => user, "course_name" => course_name, "get_all" => get_all} = Jason.decode!(body)
    result = case get_all do
	       false -> Schoolhub.CourseServer.get_affiliation(user, course_name)
	       true -> Schoolhub.CourseServer.get_all_affiliation(user, course_name)
	     end

    {code, response_body} = result |> encode_response()
    send_resp(conn, code, response_body)
  end

  put "/set_affiliation" do
    body = read_body(conn, "PUT", "/set_affiliation")

    %{"self" => self, "target" => target,
      "course_name" => course_name, "affiliation" => aff} = Jason.decode!(body)
    result = Schoolhub.CourseServer.set_affiliation(self, target, course_name, aff)

    {code, response_body} = result |> encode_response()
    send_resp(conn, code, response_body)
  end

  put "/invite_student" do
    body = read_body(conn, "PUT", "/invite_student")

    %{"self" => self, "target" => target, "course_name" => course_name} = Jason.decode!(body)
    result = Schoolhub.CourseServer.invite_student(self, target, course_name)

    {code, response_body} = result |> encode_response()
    send_resp(conn, code, response_body)
  end

  put "/remove_student" do
    body = read_body(conn, "PUT", "/remove_student")

    %{"self" => self, "target" => target, "course_name" => course_name} = Jason.decode!(body)
    result = Schoolhub.CourseServer.remove_student(self, target, course_name)

    {code, response_body} = result |> encode_response()
    send_resp(conn, code, response_body)
  end
  

  match _ do
    send_resp(conn, 404, "Oops!")
  end


  defp read_body(conn, type, url) do
    {:ok, body, _conn} = Plug.Conn.read_body(conn)
    Logger.debug("Received " <> type <> " request on " <> url <> " with body: #{inspect(body)}")
    body
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
    
    code = 200
    {code, result |> Jason.encode!()}
  end
  
end
