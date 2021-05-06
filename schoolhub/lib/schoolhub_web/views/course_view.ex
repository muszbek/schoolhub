defmodule SchoolhubWeb.CourseView do
  use SchoolhubWeb, :view

  alias Plug.Conn
  alias Schoolhub.Courses


  def get_self_aff(conn, course_id) do
    user_id = Conn.get_session(conn, :user_id)
    _affiliation = Courses.get_affiliation_by_user!(course_id, user_id)
  end
  
end
