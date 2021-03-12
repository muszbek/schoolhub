defmodule SchoolhubWeb.FollowController do
  use SchoolhubWeb, :controller
  
  alias Schoolhub.Questions


  def follow(conn, attrs = %{"course_id" => course_id, "question_id" => _question_id}) do
    user_id = get_user_id(conn)
    
    attrs
    |> Map.put("user_id", user_id)
    |> Morphix.atomorphify!()
    |> Questions.create_follow()
    
    msg = "Question followed."

    conn
    |> put_flash(:info, msg)
    |> redirect(to: Routes.course_question_path(conn, :index, course_id))
  end

  def unfollow(conn, %{"course_id" => course_id, "question_id" => question_id}) do
    user_id = get_user_id(conn)
    result = Questions.get_follow(question_id, user_id)

    case result do
      [] -> :ok
      [follow] -> {:ok, _follow} = Questions.delete_follow(follow)
    end

    msg = "Question unfollowed."

    conn
    |> put_flash(:info, msg)
    |> redirect(to: Routes.course_question_path(conn, :index, course_id))
  end

  
  defp get_user_id(conn) do
    get_session(conn, :user_id)
  end
end
