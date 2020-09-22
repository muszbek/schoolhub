defmodule SchoolhubWeb.AffiliationController do
  use SchoolhubWeb, :controller

  alias Schoolhub.Courses
  alias Schoolhub.Courses.Affiliation

  def index(conn, %{"course_id" => course_id}) do
    course_affiliations = Courses.list_course_affiliations()
    render(conn, "index.html", course_affiliations: course_affiliations,
      course_id: course_id)
  end

  def new(conn, %{"course_id" => course_id}) do
    changeset = Courses.change_affiliation(%Affiliation{})
    render(conn, "new.html", changeset: changeset,
      course_id: course_id)
  end

  def create(conn, %{"course_id" => course_id, "affiliation" => affiliation_params}) do
    case Courses.create_affiliation(affiliation_params) do
      {:ok, affiliation} ->
        conn
        |> put_flash(:info, "Affiliation created successfully.")
        |> redirect(to: Routes.course_affiliation_path(conn, :show, course_id, affiliation))

      {:error, %Ecto.Changeset{} = changeset} ->
        render(conn, "new.html", changeset: changeset,
	  course_id: course_id)
    end
  end

  def show(conn, %{"course_id" => course_id, "id" => id}) do
    affiliation = Courses.get_affiliation!(id)
    render(conn, "show.html", affiliation: affiliation,
      course_id: course_id)
  end

  def edit(conn, %{"course_id" => course_id, "id" => id}) do
    affiliation = Courses.get_affiliation!(id)
    changeset = Courses.change_affiliation(affiliation)
    render(conn, "edit.html", affiliation: affiliation, changeset: changeset,
      course_id: course_id)
  end

  def update(conn, %{"course_id" => course_id, "id" => id, "affiliation" => affiliation_params}) do
    affiliation = %{affiliation: aff_level} = Courses.get_affiliation!(id)

    case aff_level do
      "owner" ->
	conn
        |> put_flash(:error, "You can only change the owner by promoting another member!")
        |> redirect(to: Routes.course_affiliation_path(conn, :show, course_id, affiliation))
      _other ->
	do_update(conn, course_id, affiliation, affiliation_params)
    end
  end

  defp do_update(conn, course_id, affiliation, affiliation_params) do
    case Courses.update_affiliation(affiliation, affiliation_params) do
      {:ok, affiliation} ->
        conn
        |> put_flash(:info, "Affiliation updated successfully.")
        |> redirect(to: Routes.course_affiliation_path(conn, :show, course_id, affiliation))

      {:error, %Ecto.Changeset{} = changeset} ->
        render(conn, "edit.html", affiliation: affiliation, changeset: changeset,
	  course_id: course_id)
    end
  end

  def delete(conn, %{"course_id" => course_id, "id" => id}) do
    affiliation = %{affiliation: aff_level} = Courses.get_affiliation!(id)

    case aff_level do
      "owner" ->
	conn
	|> put_flash(:error, "Cannot remove the owner of the course!")
	|> redirect(to: Routes.course_affiliation_path(conn, :index, course_id))
      _other ->
	{:ok, _affiliation} = Courses.delete_affiliation(affiliation)

	conn
	|> put_flash(:info, "Affiliation deleted successfully.")
	|> redirect(to: Routes.course_affiliation_path(conn, :index, course_id))
    end
  end
end
