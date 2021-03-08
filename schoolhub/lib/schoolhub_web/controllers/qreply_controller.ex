defmodule SchoolhubWeb.QreplyController do
  use SchoolhubWeb, :controller

  alias Schoolhub.Questions
  alias Schoolhub.Questions.Qreply

  def index(conn, _params) do
    question_replies = Questions.list_question_replies()
    render(conn, "index.html", question_replies: question_replies)
  end

  def new(conn, _params) do
    changeset = Questions.change_qreply(%Qreply{})
    render(conn, "new.html", changeset: changeset)
  end

  def create(conn, %{"qreply" => qreply_params}) do
    case Questions.create_qreply(qreply_params) do
      {:ok, qreply} ->
        conn
        |> put_flash(:info, "Qreply created successfully.")
        |> redirect(to: Routes.qreply_path(conn, :show, qreply))

      {:error, %Ecto.Changeset{} = changeset} ->
        render(conn, "new.html", changeset: changeset)
    end
  end

  def show(conn, %{"id" => id}) do
    qreply = Questions.get_qreply!(id)
    render(conn, "show.html", qreply: qreply)
  end

  def edit(conn, %{"id" => id}) do
    qreply = Questions.get_qreply!(id)
    changeset = Questions.change_qreply(qreply)
    render(conn, "edit.html", qreply: qreply, changeset: changeset)
  end

  def update(conn, %{"id" => id, "qreply" => qreply_params}) do
    qreply = Questions.get_qreply!(id)

    case Questions.update_qreply(qreply, qreply_params) do
      {:ok, qreply} ->
        conn
        |> put_flash(:info, "Qreply updated successfully.")
        |> redirect(to: Routes.qreply_path(conn, :show, qreply))

      {:error, %Ecto.Changeset{} = changeset} ->
        render(conn, "edit.html", qreply: qreply, changeset: changeset)
    end
  end

  def delete(conn, %{"id" => id}) do
    qreply = Questions.get_qreply!(id)
    {:ok, _qreply} = Questions.delete_qreply(qreply)

    conn
    |> put_flash(:info, "Qreply deleted successfully.")
    |> redirect(to: Routes.qreply_path(conn, :index))
  end
end
