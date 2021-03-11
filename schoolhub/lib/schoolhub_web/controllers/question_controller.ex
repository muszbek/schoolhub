defmodule SchoolhubWeb.QuestionController do
  use SchoolhubWeb, :controller
  
  alias Schoolhub.Questions
  alias Schoolhub.Questions.Question

  @question_limit_default "5"

  def index(conn, %{"course_id" => course_id, "limit" => limit}) do
    questions = Questions.list_course_questions(course_id, limit)
    limit = if Enum.count(questions) < String.to_integer(limit), do: -1, else: limit
    render(conn, "index.html", questions: questions, course_id: course_id, question_limit: limit, filters: [])
  end
  def index(conn, %{"course_id" => course_id}) do
    index(conn, %{"course_id" => course_id, "limit" => @question_limit_default})
  end

  def filter(conn, %{"course_id" => course_id, "filters" => filters_string}) do
    filters_list = String.split(filters_string, "@", [trim: true])
    questions = Questions.filter_questions(course_id, filters_list)
    render(conn, "index.html", questions: questions, course_id: course_id, question_limit: -1, filters: filters_list)
  end

  def new(conn, %{"course_id" => course_id}) do
    changeset = Questions.change_question(%Question{})
    render(conn, "new.html", changeset: changeset, course_id: course_id)
  end

  def create(conn, %{"course_id" => course_id, "question" => question_params}) do
    user_id = get_session(conn, :user_id)
    question_params_extended = question_params
    |> Map.put("creator", user_id)
    |> Map.put("pinned", false)
    |> Map.update("tags", [], &(String.split(&1, " ", [trim: true])))
    |> Morphix.atomorphify!()
    
    case Questions.create_question(question_params_extended) do
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
    render(conn, "show.html", question: question, course_id: course_id, qreplies: question.qreply)
  end

  def edit(conn, %{"course_id" => course_id, "id" => id}) do
    question = Questions.get_question!(id)
    changeset = Questions.change_question(question)
    render(conn, "edit.html", question: question, changeset: changeset, course_id: course_id)
  end

  def update(conn, %{"course_id" => course_id, "id" => id, "question" => question_params}) do
    question = Questions.get_question!(id)
    question_params_extended = question_params
    |> Map.update("tags", [], &(String.split(&1, " ", [trim: true])))
    |> Morphix.atomorphify!()

    case Questions.update_question(question, question_params_extended) do
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
    
    msg = if to_pin == "true", do: "Question pinned.", else: "Question unpinned."
    
    conn
    |> put_flash(:info, msg)
    |> redirect(to: Routes.course_question_path(conn, :index, course_id))
  end
end
