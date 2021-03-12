defmodule SchoolhubWeb.QuestionView do
  use SchoolhubWeb, :view

  alias Plug.Conn
  alias Schoolhub.Questions

  
  def render_tag(taglist \\ []) do
    Enum.join(taglist, " ")
  end

  def is_follow(conn, question) do
    user_id = Conn.get_session(conn, :user_id)
    question_id = question.id
    result = Questions.get_follow(question_id, user_id)

    case result do
      [] -> false
      [_follow] -> true
    end
  end
end
