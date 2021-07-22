defmodule SchoolhubWeb.QreplyController do
  use SchoolhubWeb, :controller

  alias Schoolhub.Questions
  alias Schoolhub.Questions.Qreply

  def index(conn, %{"course_id" => course_id, "question_id" => question_id}) do
    redirect(conn, to: Routing.route(:course_question_path, conn, [:show, course_id, question_id]))
  end

  def new(conn, %{"course_id" => course_id, "question_id" => question_id}) do
    changeset = Questions.change_qreply(%Qreply{})
    render(conn, "new.html", changeset: changeset,
      course_id: course_id, question_id: question_id)
  end

  def create(conn, %{"course_id" => course_id, "question_id" => question_id, "qreply" => qreply_params}) do
    user_id = get_session(conn, :user_id)
    qreply_params_with_creator = qreply_params
    |> Map.put("creator", user_id)
    |> Map.put("parent_question", question_id)
    |> Morphix.atomorphify!()

    case Questions.create_qreply(qreply_params_with_creator) do
      {:ok, qreply} ->
        conn
        |> put_flash(:info, "Reply created successfully.")
        |> redirect(to: Routing.route(:course_question_qreply_path, conn, [:show, course_id, question_id, qreply]))

      {:error, %Ecto.Changeset{} = changeset} ->
        render(conn, "new.html", changeset: changeset,
	  course_id: course_id, question_id: question_id)
    end
  end

  def show(conn, %{"course_id" => course_id, "question_id" => question_id, "id" => id}) do
    qreply = Questions.get_qreply!(id)
    render(conn, "show.html", qreply: qreply, course_id: course_id, question_id: question_id)
  end

  def edit(conn, %{"course_id" => course_id, "question_id" => question_id, "id" => id}) do
    qreply = Questions.get_qreply!(id)
    changeset = Questions.change_qreply(qreply)
    render(conn, "edit.html", qreply: qreply, changeset: changeset,
      course_id: course_id, question_id: question_id)
  end

  def update(conn, %{"course_id" => course_id, "question_id" => question_id,
		     "id" => id, "qreply" => qreply_params}) do
    qreply = Questions.get_qreply!(id)

    case Questions.update_qreply(qreply, qreply_params) do
      {:ok, qreply} ->
        conn
        |> put_flash(:info, "Reply updated successfully.")
        |> redirect(to: Routing.route(:course_question_qreply_path, conn, [:show, course_id, question_id, qreply]))

      {:error, %Ecto.Changeset{} = changeset} ->
        render(conn, "edit.html", qreply: qreply, changeset: changeset,
	  course_id: course_id, question_id: question_id)
    end
  end

  def delete(conn, %{"course_id" => course_id, "question_id" => question_id, "id" => id}) do
    qreply = Questions.get_qreply!(id)
    {:ok, _qreply} = Questions.delete_qreply(qreply)

    conn
    |> put_flash(:info, "Reply deleted successfully.")
    |> redirect(to: Routing.route(:course_question_qreply_path, conn, [:index, course_id, question_id]))
  end
end
