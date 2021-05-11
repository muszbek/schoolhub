defmodule SchoolhubWeb.CourseController do
  use SchoolhubWeb, :controller

  alias File, as: BuiltinFile
  alias Schoolhub.Courses
  alias Schoolhub.Courses.Course

  def admin_index(conn, _params) do
    courses = Courses.list_courses()
    render(conn, "index.html", courses: courses)
  end
  
  def index(conn, _params) do
    user_id = get_session(conn, :user_id)
    courses = Courses.list_affiliated_courses(user_id)
    render(conn, "index.html", courses: courses)
  end

  def new(conn, _params) do
    changeset = Courses.change_course(%Course{})
    render(conn, "new.html", changeset: changeset)
  end

  def create(conn, %{"course" => course_params}) do
    user_id = get_session(conn, :user_id)
    picture = Map.get(course_params, "picture", nil)
    binary_content = case picture do
		       nil -> nil
		       file -> BuiltinFile.read!(file.path)
		     end
    
    course_params_with_creator = course_params
    |> Map.put("creator", user_id)
    |> Map.put("picture", binary_content)
    |> Morphix.atomorphify!()
    
    case Courses.create_course(course_params_with_creator) do
      {:ok, course = %{id: course_id}} ->
	%{course_id: course_id, user_id: user_id, affiliation: "owner"}
	|> Courses.create_affiliation()
	
        conn
        |> put_flash(:info, "Course created successfully.")
        |> redirect(to: Routes.course_path(conn, :show, course))

      {:error, %Ecto.Changeset{} = changeset} ->
        render(conn, "new.html", changeset: changeset)
    end
  end

  def show(conn, %{"id" => id}) do
    course = Courses.get_course!(id)
    render(conn, "show.html", course: course)
  end

  def edit(conn, %{"id" => id}) do
    course = Courses.get_course!(id)
    changeset = Courses.change_course(course)
    render(conn, "edit.html", course: course, changeset: changeset)
  end

  def update(conn, %{"id" => id, "course" => course_params}) do
    course = Courses.get_course!(id)
    picture = Map.get(course_params, "picture", nil)
    binary_content = case picture do
		       nil -> nil
		       file -> BuiltinFile.read!(file.path)
		     end
    
    course_params_with_picture = course_params
    |> Map.put("picture", binary_content)
    |> Morphix.atomorphify!()

    case Courses.update_course(course, course_params_with_picture) do
      {:ok, course} ->
        conn
        |> put_flash(:info, "Course updated successfully.")
        |> redirect(to: Routes.course_path(conn, :show, course))

      {:error, %Ecto.Changeset{} = changeset} ->
        render(conn, "edit.html", course: course, changeset: changeset)
    end
  end

  def delete(conn, %{"id" => id}) do
    course = Courses.get_course!(id)
    {:ok, _course} = Courses.delete_course(course)

    conn
    |> put_flash(:info, "Course deleted successfully.")
    |> redirect(to: Routes.course_path(conn, :index))
  end

  def new_token(conn, %{"course_id" => id}) do
    course = Courses.get_course!(id)
    token = Courses.create_token(course.id)
    render(conn, "new_token.html", course: course, token: token)
  end

  def activate(conn, %{"course_id" => id, "to_activate" => to_activate}) do
    course = Courses.get_course!(id)
    {:ok, _course} = Courses.update_course(course, %{active: to_activate})

    msg = if to_activate == "true", do: "Course activated.", else: "Course disabled."

    conn
    |> put_flash(:info, msg)
    |> redirect(to: Routes.course_path(conn, :show, id))
  end
  
end
