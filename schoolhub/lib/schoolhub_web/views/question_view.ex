defmodule SchoolhubWeb.QuestionView do
  use SchoolhubWeb, :view

  alias Schoolhub.Questions
  
  def render_tag(taglist \\ []) do
    Enum.join(taglist, " ")
  end

  def render_content(user_id, question) do
    content = question.content
    |> SchoolhubWeb.Gettext.first_line()

    follow_tag = if is_follow(user_id, question), do: " << Following >> ", else: ""
    
    follow_tag <> content
  end

  def is_follow(user_id, question) do
    question_id = question.id
    result = Questions.get_follow(question_id, user_id)

    case result do
      [] -> false
      [_follow] -> true
    end
  end

  def checkbox_follow("all"), do: ""
  def checkbox_follow("only_following"), do: "checked"
end
