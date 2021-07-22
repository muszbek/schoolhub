defmodule SchoolhubWeb.GradeController do
  use SchoolhubWeb, :controller
  
  alias Schoolhub.{Accounts, Courses, Grades}
  alias Schoolhub.Grades.Grade

  def index(conn, %{"course_id" => course_id}) do
    grades = Grades.list_grades(course_id)
    displayed_grades = Enum.map(grades, &create_grade_index_display/1)
    render(conn, "index.html", course_id: course_id, grades: displayed_grades)
  end

  defp create_grade_index_display(grade) do
    grade_json = display_grade(grade)
    name = grade.affiliation_id
    |> Courses.get_affiliation!()
    |> Map.fetch!(:user_id)
    |> Accounts.get_user!()
    |> Map.fetch!(:name)

    Map.merge(grade_json, %{name: name})
  end

  def new(conn, %{"course_id" => course_id, "affiliation_id" => aff_id}) do
    changeset = Grades.change_grade(%Grade{})
    render(conn, "new.html", changeset: changeset,
      course_id: course_id, affiliation_id: aff_id)
  end

  def create(conn, %{"course_id" => course_id, "affiliation_id" => aff_id,
		     "title" => title, "grade" => grade}) do

    grade_id = Courses.get_affiliation!(aff_id).grade.id
    grade_input = Morphix.atomorphify!(%{title => grade})
    
    case Grades.add_grade(grade_id, grade_input) do
      {:ok, grade} ->
        conn
        |> put_flash(:info, "Grade created successfully.")
        |> redirect(to: Routing.route(:course_affiliation_grade_path, conn, [:show, course_id, aff_id, grade]))

      {:error, %Ecto.Changeset{} = changeset} ->
        render(conn, "new.html", changeset: changeset,
	  course_id: course_id, affiliation_id: aff_id)
    end
  end

  def show(conn, %{"course_id" => course_id, "affiliation_id" => aff_id, "id" => id}) do
    grade = Grades.get_grade!(id)
    grade_string = display_grade(grade)
    render(conn, "show.html", grade: grade_string,
      course_id: course_id, affiliation_id: aff_id)
  end

  def show_self(conn, %{"course_id" => course_id, "affiliation_id" => aff_id}) do
    %{grade: grade} = Courses.get_affiliation!(aff_id)
    grade_string = display_grade(grade)
    render(conn, "show_self.html", grade: grade_string, course_id: course_id)
  end

  def edit(conn, %{"course_id" => course_id, "affiliation_id" => aff_id, "id" => id}) do
    grade = Grades.get_grade!(id)
    grade_string = display_grade(grade)
    changeset = Grades.change_grade(grade_string)
    render(conn, "edit.html", grade: grade_string, changeset: changeset,
      course_id: course_id, affiliation_id: aff_id)
  end

  def update(conn, %{"course_id" => course_id, "affiliation_id" => aff_id,
		     "id" => id, "grade" => grade_params}) do
    grade_params
    |> decode_grade()
    |> do_update(conn, course_id, aff_id, id)
  end

  defp do_update(grade_params_map = %{}, conn, course_id, aff_id, id) do
    grade = Grades.get_grade!(id)

    case Grades.update_grade(grade, grade_params_map) do
      {:ok, grade} ->
        conn
        |> put_flash(:info, "Grade updated successfully.")
        |> redirect(to: Routing.route(:course_affiliation_grade_path, conn, [:show, course_id, aff_id, grade]))

      {:error, %Ecto.Changeset{} = changeset} ->
        render(conn, "edit.html", grade: grade, changeset: changeset,
	  course_id: course_id, affiliation_id: aff_id)
    end
  end
  defp do_update({:json_error, _wrong_grade_string}, conn, course_id, aff_id, id) do
    grade = Grades.get_grade!(id)
    grade_string = display_grade(grade)
    changeset = Grades.change_grade(grade_string)

    conn
    |> put_flash(:error, "Wrong json format!")
    |> render("edit.html", grade: grade_string, changeset: changeset,
      course_id: course_id, affiliation_id: aff_id)
  end

  def delete(conn, %{"course_id" => course_id, "id" => id}) do
    grade = Grades.get_grade!(id)
    {:ok, _grade} = Grades.reset_grade(grade)

    conn
    |> put_flash(:info, "Grade deleted successfully.")
    |> redirect(to: Routing.route(:course_grade_path, conn, [:index, course_id]))
  end


  defp display_grade(grade = %{grades: map}) do
    json = Jason.encode!(map, pretty: true)
    %{grade | grades: json}
  end
  
  defp decode_grade(grade = %{"grades" => json}) do
    try do
      map = Jason.decode!(json)
      %{grade | "grades" => map}
    rescue
      Jason.DecodeError -> {:json_error, grade}
    end
  end
end
