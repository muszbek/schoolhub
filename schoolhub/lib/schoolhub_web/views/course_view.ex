defmodule SchoolhubWeb.CourseView do
  use SchoolhubWeb, :view

  alias Plug.Conn
  alias Schoolhub.{Courses, Grades}


  def get_self_aff(conn, course_id) do
    user_id = Conn.get_session(conn, :user_id)
    _affiliation = Courses.get_affiliation_by_user!(course_id, user_id)
  end
  
  def get_self_grades(conn, course_id) do
    affiliation = get_self_aff(conn, course_id)
    _grade = Grades.get_grade_by_aff!(affiliation.id)
  end
end
