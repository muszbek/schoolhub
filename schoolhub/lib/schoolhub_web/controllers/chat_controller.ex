defmodule SchoolhubWeb.ChatController do
  use SchoolhubWeb, :controller

  alias Schoolhub.{Accounts, Courses}
  
  def index(conn, %{"course_id" => course_id}) do
    users = Courses.list_affiliated_users(course_id)
    render(conn, "index.html", course_id: course_id, users: users)
  end

  def chat(conn, %{"course_id" => course_id, "user_id" => user_id}) do
    user = Accounts.get_user!(user_id)
    render(conn, "chat.html", course_id: course_id, user: user)
  end

end
