defmodule SchoolhubWeb.QuestionLive do
  use Phoenix.LiveView

  alias Schoolhub.Questions
  require Logger

  @question_limit_def 5
  @questions_increment 5

  def mount(_params, %{"internal_host" => host, "course_id" => course_id, "user_id" => user_id},
    socket) do
    
    questions = Questions.list_course_questions(course_id, @question_limit_def)
    limit = if Enum.count(questions) < @question_limit_def, do: -1, else: @question_limit_def
    assigns = [host: host, questions: questions, course_id: course_id, user_id: user_id,
	       question_limit: limit, filters: [], only_following: "all"]
    {:ok, assign(socket, assigns)}
  end
  
  def render(assigns) do
    SchoolhubWeb.QuestionView.render("index.html", assigns)
  end

  def handle_event("only_following_changed", value, socket) do
    assigns = socket.assigns
    only_following = parse_following(value)
    questions = Questions.filter_questions(assigns.course_id, assigns.user_id,
      assigns.filters, only_following, @question_limit_def)
    limit = if Enum.count(questions) < @question_limit_def, do: -1, else: @question_limit_def
    new_assigns = [questions: questions, only_following: only_following, question_limit: limit]
    {:noreply, assign(socket, new_assigns)}
  end

  def handle_event("more_clicked", _value, socket) do
    assigns = socket.assigns
    new_limit = assigns.question_limit + @questions_increment
    questions = Questions.filter_questions(assigns.course_id, assigns.user_id,
      assigns.filters, assigns.only_following, new_limit)
    limit = if Enum.count(questions) < new_limit, do: -1, else: new_limit
    new_assigns = [questions: questions, question_limit: limit]
    {:noreply, assign(socket, new_assigns)}
  end

  def handle_event("filter", %{"filter_field" => value}, socket) do
    Logger.warn(inspect(value))
    assigns = socket.assigns
    filters_list = String.split(value, " ", [trim: true])
    questions = Questions.filter_questions(assigns.course_id, assigns.user_id,
      filters_list, assigns.only_following, @question_limit_def)
    limit = if Enum.count(questions) < @question_limit_def, do: -1, else: @question_limit_def
    new_assigns = [questions: questions, filters: filters_list, question_limit: limit]
    {:noreply, assign(socket, new_assigns)}
  end

  
  defp parse_following(%{"value" => "on"}), do: "only_following"
  defp parse_following(%{}), do: "all"
  
end
