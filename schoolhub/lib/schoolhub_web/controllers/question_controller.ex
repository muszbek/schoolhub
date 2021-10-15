defmodule SchoolhubWeb.QuestionController do
  use SchoolhubWeb, :controller

  alias Plug.Conn
  alias Schoolhub.Questions
  alias Schoolhub.Questions.Question
  
  def index(conn, %{"course_id" => course_id}) do
    host = Routing.internal_host(conn)
    user_id = Conn.get_session(conn, :user_id)
    session = %{"internal_host" => host, "course_id" => course_id, "user_id" => user_id}
    live_render(conn, SchoolhubWeb.QuestionLive, session: session)
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
        |> redirect(to: Routing.route(:course_question_path, conn, [:show, course_id, question]))

      {:error, %Ecto.Changeset{} = changeset} ->
        render(conn, "new.html", changeset: changeset, course_id: course_id)
    end
  end

  def show(conn, %{"course_id" => course_id, "id" => id}) do
    user_id = get_session(conn, :user_id)
    question = Questions.get_question!(id)
    render(conn, "show.html", question: question, course_id: course_id, user_id: user_id,
      qreplies: question.qreply)
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
        |> redirect(to: Routing.route(:course_question_path, conn, [:show, course_id, question]))

      {:error, %Ecto.Changeset{} = changeset} ->
        render(conn, "edit.html", question: question, changeset: changeset, course_id: course_id)
    end
  end

  def delete(conn, %{"course_id" => course_id, "id" => id}) do
    question = Questions.get_question!(id)
    {:ok, _question} = Questions.delete_question(question)

    conn
    |> put_flash(:info, "Question deleted successfully.")
    |> redirect(to: Routing.route(:course_question_path, conn, [:index, course_id]))
  end

  def pin(conn, %{"course_id" => course_id, "question_id" => id, "to_pin" => to_pin}) do
    
    question = Questions.get_question!(id)
    {:ok, _question} = Questions.update_question(question, %{pinned: to_pin})
    
    msg = if to_pin == "true", do: "Question pinned.", else: "Question unpinned."
    
    conn
    |> put_flash(:info, msg)
    |> redirect(to: Routing.route(:course_question_path, conn, [:index, course_id]))
  end
  
end
