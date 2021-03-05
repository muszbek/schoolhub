defmodule SchoolhubWeb.QuestionController do
  use SchoolhubWeb, :controller

  alias Schoolhub.Questions
  alias Schoolhub.Questions.Question

  def index(conn, %{"course_id" => course_id}) do
    questions = Questions.list_questions()
    render(conn, "index.html", questions: questions, course_id: course_id)
  end

  def new(conn, %{"course_id" => course_id}) do
    changeset = Questions.change_question(%Question{})
    render(conn, "new.html", changeset: changeset, course_id: course_id)
  end

  def create(conn, %{"course_id" => course_id, "question" => question_params}) do
    user_id = get_session(conn, :user_id)
    question_params_with_creator = question_params
    |> Map.put("creator", user_id)
    |> Map.put("pinned", false)
    |> Map.put("tags", [])
    |> Morphix.atomorphify!()
    
    case Questions.create_question(question_params_with_creator) do
      {:ok, question} ->
        conn
        |> put_flash(:info, "Question created successfully.")
        |> redirect(to: Routes.course_question_path(conn, :show, course_id, question))

      {:error, %Ecto.Changeset{} = changeset} ->
        render(conn, "new.html", changeset: changeset, course_id: course_id)
    end
  end

  def show(conn, %{"course_id" => course_id, "id" => id}) do
    question = Questions.get_question!(id)
    render(conn, "show.html", question: question, course_id: course_id)
  end

  def edit(conn, %{"course_id" => course_id, "id" => id}) do
    question = Questions.get_question!(id)
    changeset = Questions.change_question(question)
    render(conn, "edit.html", question: question, changeset: changeset, course_id: course_id)
  end

  def update(conn, %{"course_id" => course_id, "id" => id, "question" => question_params}) do
    question = Questions.get_question!(id)

    case Questions.update_question(question, question_params) do
      {:ok, question} ->
        conn
        |> put_flash(:info, "Question updated successfully.")
        |> redirect(to: Routes.course_question_path(conn, :show, course_id, question))

      {:error, %Ecto.Changeset{} = changeset} ->
        render(conn, "edit.html", question: question, changeset: changeset, course_id: course_id)
    end
  end

  def delete(conn, %{"course_id" => course_id, "id" => id}) do
    question = Questions.get_question!(id)
    {:ok, _question} = Questions.delete_question(question)

    conn
    |> put_flash(:info, "Question deleted successfully.")
    |> redirect(to: Routes.course_question_path(conn, :index, course_id))
  end

  def pin(conn, %{"course_id" => course_id, "question_id" => id, "to_pin" => to_pin}) do
    
    question = Questions.get_question!(id)
    {:ok, _question} = Questions.update_question(question, %{pinned: to_pin})
    
    msg = if to_pin, do: "Question pinned.", else: "Question unpinned."
    
    conn
    |> put_flash(:info, msg)
    |> redirect(to: Routes.course_question_path(conn, :index, course_id))
  end
end
