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
    rest_json_reply(conn, result)
  end

  delete "/users" do
    body = get_body(conn)
    
    %{"username" => username} = Jason.decode!(body)
    result = Schoolhub.RegServer.remove_user(username)
    rest_json_reply(conn, result)
  end
  
  get "/users/privileges" do
    body = get_body(conn)

    %{"get_all" => get_all, "self" => self} = Jason.decode!(body)
    result = case get_all do
	       false -> Schoolhub.RegServer.get_user_privilege(self)
	       true -> Schoolhub.RegServer.get_all_privilege(self)
	     end
    rest_json_reply(conn, result)
  end

  put "/users/privileges" do
    body = get_body(conn)

    %{"self" => self, "target" => target, "privilege" => priv} = Jason.decode!(body)
    result = Schoolhub.RegServer.set_user_privilege(self, target, priv)
    rest_json_reply(conn, result)
  end

  
  get "/get_mam" do
    body = get_body(conn)

    %{"self" => self, "partner" => partner, "limit" => limit} = Jason.decode!(body)
    result = Schoolhub.ChatServer.get_archive(self, partner, limit)
    rest_json_reply(conn, result)
  end


  post "/courses" do
    body = get_body(conn)

    %{"self" => self, "course_name" => course_name} = Jason.decode!(body)
    result = Schoolhub.CourseAdminServer.create_course(self, course_name)
    rest_json_reply(conn, result)
  end

  delete "/courses" do
    body = get_body(conn)

    %{"self" => self, "course_name" => course_name} = Jason.decode!(body)
    result = Schoolhub.CourseAdminServer.remove_course(self, course_name)
    rest_json_reply(conn, result)
  end

  
  post "/courses/students" do
    body = get_body(conn)

    %{"self" => self, "target" => target, "course_name" => course_name} = Jason.decode!(body)
    result = Schoolhub.CourseAdminServer.invite_student(self, target, course_name)
    rest_json_reply(conn, result)
  end

  delete "/courses/students" do
    body = get_body(conn)

    %{"self" => self, "target" => target, "course_name" => course_name} = Jason.decode!(body)
    result = Schoolhub.CourseAdminServer.remove_student(self, target, course_name)
    rest_json_reply(conn, result)
  end
  
  get "/courses/students/affiliations" do
    body = get_body(conn)

    %{"self" => user, "course_name" => course_name, "get_all" => get_all} = Jason.decode!(body)
    result = case get_all do
	       false -> Schoolhub.CourseAdminServer.get_affiliation(user, course_name)
	       true -> Schoolhub.CourseAdminServer.get_all_affiliation(user, course_name)
	     end
    rest_json_reply(conn, result)
  end

  put "/courses/students/affiliations" do
    body = get_body(conn)

    %{"self" => self, "target" => target,
      "course_name" => course_name, "affiliation" => aff} = Jason.decode!(body)
    result = Schoolhub.CourseAdminServer.set_affiliation(self, target, course_name, aff)
    rest_json_reply(conn, result)
  end

  get "/courses/desc" do
    body = get_body(conn)

    %{"course_name" => course_name} = Jason.decode!(body)
    result = Schoolhub.CourseContentServer.get_description(course_name)
    rest_json_reply(conn, result)
  end

  put "/courses/desc" do
    body = get_body(conn)

    %{"self" => self, "course_name" => course_name, "description" => desc} = Jason.decode!(body)
    result = Schoolhub.CourseContentServer.set_description(self, course_name, desc)
    rest_json_reply(conn, result)
  end

  post "/courses/messages" do
    body = get_body(conn)

    %{"self" => self, "id" => id, "course_name" => course_name, "message" => message} =
      Jason.decode!(body)
    
    result = case id do
	       nil -> Schoolhub.CourseContentServer.post_message(self, course_name, message)
	       id -> Schoolhub.CourseContentServer.post_reply(id, self, course_name, message)
	     end
    rest_json_reply(conn, result)
  end

  get "/courses/messages" do
    body = get_body(conn)

    %{"self" => self, "id" => id, "course_name" => course_name, "number" => number} =
      Jason.decode!(body)
    
    result = case number do
	       1 -> Schoolhub.CourseContentServer.get_single_message(id, self, course_name)
	       bigger -> Schoolhub.CourseContentServer.get_replies(id, self, course_name, bigger)
	     end
    rest_json_reply(conn, result)
  end

  delete "/courses/messages" do
    body = get_body(conn)

    %{"self" => self, "id" => id, "course_name" => course_name, "all" => all} = Jason.decode!(body)
    result = case all do
	       false -> Schoolhub.CourseContentServer.delete_single_message(id, self, course_name)
	       true -> Schoolhub.CourseContentServer.delete_root_message(id, self, course_name)
	     end
    rest_json_reply(conn, result)
  end

  put "/courses/messages" do
    body = get_body(conn)

    %{"self" => self, "id" => id, "course_name" => course_name, "message" => message} =
      Jason.decode!(body)
    result = Schoolhub.CourseContentServer.modify_single_message(id, self, course_name, message)
    rest_json_reply(conn, result)
  end

  get "/courses/message_board" do
    body = get_body(conn)

    %{"self" => self, "course_name" => course_name, "number" => number} = Jason.decode!(body)
    result = Schoolhub.CourseContentServer.get_root_messages(self, course_name, number)
    rest_json_reply(conn, result)
  end

  put "/courses/messages/pin" do
    body = get_body(conn)

    %{"self" => self, "id" => id, "course_name" => course_name, "pinned" => pinned} =
      Jason.decode!(body)
    result = Schoolhub.CourseContentServer.pin_message(id, self, course_name, pinned)
    rest_json_reply(conn, result)
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
		       {:ok, id} when is_integer(id) -> {200, %{"id" => id}}
		       {:ok, _reason} -> {200, "ok"}
		       :ok -> {200, "ok"}
		       {:error, :no_permission} -> {401, "ERROR_no_permission"}
		       {:error, reason} -> {403, "ERROR_" <> Atom.to_string(reason)}
		       other -> {200, other}
		     end
    
    {code, result |> Jason.encode!()}
  end

  defp rest_json_reply(conn, result) do
    {code, response_body} = result |> encode_response()
    send_resp(conn, code, response_body)
  end
  
end
