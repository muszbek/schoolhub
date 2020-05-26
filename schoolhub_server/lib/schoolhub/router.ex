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
    body = get_body(conn)

    Schoolhub.AuthServer.authenticate(body)
    http_respond(conn)
  end

  post "/users" do
    body = get_body(conn)
    
    %{"username" => username, "password" => password} = Jason.decode!(body)
    result = Schoolhub.RegServer.register_user(username, password)

    {code, response_body} = result |> encode_response()
    send_resp(conn, code, response_body)
  end

  delete "/users" do
    body = get_body(conn)
    
    %{"username" => username} = Jason.decode!(body)
    result = Schoolhub.RegServer.remove_user(username)

    {code, response_body} = result |> encode_response()
    send_resp(conn, code, response_body)
  end
  
  get "/privileges" do
    body = get_body(conn)

    %{"get_all" => get_all, "self" => self} = Jason.decode!(body)
    result = case get_all do
	       false -> Schoolhub.RegServer.get_user_privilege(self)
	       true -> Schoolhub.RegServer.get_all_privilege(self)
	     end

    {code, response_body} = result |> encode_response()
    send_resp(conn, code, response_body)
  end

  put "/privileges" do
    body = get_body(conn)

    %{"self" => self, "target" => target, "privilege" => priv} = Jason.decode!(body)
    result = Schoolhub.RegServer.set_user_privilege(self, target, priv)

    {code, response_body} = result |> encode_response()
    send_resp(conn, code, response_body)
  end

  
  get "/get_mam" do
    body = get_body(conn)

    %{"self" => self, "partner" => partner, "limit" => limit} = Jason.decode!(body)
    result = Schoolhub.ChatServer.get_archive(self, partner, limit)

    {code, response_body} = result |> encode_response()
    send_resp(conn, code, response_body)
  end


  post "/courses" do
    body = get_body(conn)

    %{"self" => self, "course_name" => course_name} = Jason.decode!(body)
    result = Schoolhub.CourseServer.create_course(self, course_name)

    {code, response_body} = result |> encode_response()
    send_resp(conn, code, response_body)
  end

  delete "/courses" do
    body = get_body(conn)

    %{"self" => self, "course_name" => course_name} = Jason.decode!(body)
    result = Schoolhub.CourseServer.remove_course(self, course_name)

    {code, response_body} = result |> encode_response()
    send_resp(conn, code, response_body)
  end

  get "/affiliations" do
    body = get_body(conn)

    %{"self" => user, "course_name" => course_name, "get_all" => get_all} = Jason.decode!(body)
    result = case get_all do
	       false -> Schoolhub.CourseServer.get_affiliation(user, course_name)
	       true -> Schoolhub.CourseServer.get_all_affiliation(user, course_name)
	     end

    {code, response_body} = result |> encode_response()
    send_resp(conn, code, response_body)
  end

  put "/affiliations" do
    body = get_body(conn)

    %{"self" => self, "target" => target,
      "course_name" => course_name, "affiliation" => aff} = Jason.decode!(body)
    result = Schoolhub.CourseServer.set_affiliation(self, target, course_name, aff)

    {code, response_body} = result |> encode_response()
    send_resp(conn, code, response_body)
  end

  post "/students" do
    body = get_body(conn)

    %{"self" => self, "target" => target, "course_name" => course_name} = Jason.decode!(body)
    result = Schoolhub.CourseServer.invite_student(self, target, course_name)

    {code, response_body} = result |> encode_response()
    send_resp(conn, code, response_body)
  end

  delete "/students" do
    body = get_body(conn)

    %{"self" => self, "target" => target, "course_name" => course_name} = Jason.decode!(body)
    result = Schoolhub.CourseServer.remove_student(self, target, course_name)

    {code, response_body} = result |> encode_response()
    send_resp(conn, code, response_body)
  end
  

  match _ do
    send_resp(conn, 404, "Oops!")
  end


  defp get_body(conn) do
    {:ok, body, _conn} = Plug.Conn.read_body(conn)
    Logger.debug("Received HTTP request with body: #{inspect(body)}")
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
    {code, result} = case response do
		       {:ok, _reason} -> {200, "ok"}
		       :ok -> {200, "ok"}
		       {:error, :no_permission} -> {401, "ERROR_no_permission"}
		       {:error, reason} -> {403, "ERROR_" <> Atom.to_string(reason)}
		       other -> {200, other}
		     end
    
    {code, result |> Jason.encode!()}
  end
  
end
