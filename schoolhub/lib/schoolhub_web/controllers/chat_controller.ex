defmodule SchoolhubWeb.ChatController do
  use SchoolhubWeb, :controller

  alias Schoolhub.{Accounts, Courses}
  
  def index(conn, %{"course_id" => course_id}) do
    affiliations = Courses.list_course_affiliations(course_id)
    render(conn, "index.html", course_id: course_id, affiliations: affiliations)
  end

  def chat(conn, %{"course_id" => course_id, "user_id" => user_id}) do
    user = Accounts.get_user!(user_id)
    self = conn
    |> get_session(:user_id)
    |> Accounts.get_user!()

    refresh_token = get_session(conn, :refresh_token)

    case refresh_token do
      nil ->
	conn
        |> put_flash(:error, "Token missing for XMPP authentication, please authenticate again")
        |> redirect(to: Routing.route(:session_path, conn, [:new]))
      _ ->
	render(conn, "chat.html", course_id: course_id, user: user, self: self,
	  token: refresh_token)
    end
  end

end
