defmodule SchoolhubWeb.FollowControllerTest do
  use SchoolhubWeb.ConnCase

  alias Schoolhub.{Accounts, Courses, Privileges, Questions}

  @create_user_attrs %{email: "some email",
		       name: "some name",
		       credential: %{username: "some username",
				     password: "some password"}}
  @create_course_attrs %{description: "some description", name: "some name"}
  @create_question_attrs %{content: "some question content", pinned: false}

  def fixture(:question, conn = %Plug.Conn{}) do
    {:ok, user} = Accounts.create_user(@create_user_attrs)
    Privileges.update_privilege(user.privilege, %{level: "admin"})
    
    new_conn = conn
    |> Plug.Test.init_test_session(user_id: nil)
    |> SchoolhubWeb.SessionController.enter_session(user)

    {:ok, _course = %{id: course_id}} = Courses.create_course(@create_course_attrs)
    aff_attrs = %{course_id: course_id, user_id: user.id, affiliation: "owner"}
    {:ok, _aff} = Courses.create_affiliation(aff_attrs)

    {:ok, question} = @create_question_attrs
    |> Map.put("course_id", course_id)
    |> Map.put("creator", user.id)
    |> Morphix.atomorphify!()
    |> Questions.create_question()

    %{conn: new_conn, course_id: course_id, question_id: question.id, user_id: user.id}
  end
  
  def fixture(:follow, question_id, user_id) do
    follow_attrs = create_valid_attrs(question_id, user_id)
    {:ok, follow} = Questions.create_follow(follow_attrs)
    follow
  end

  
  describe "follow question" do
    setup [:create_question]

    test "follows question", %{conn: conn, course_id: course_id, question_id: question_id} do
      conn = post(conn, Routes.course_question_follow_path(conn, :follow, course_id, question_id))
      assert redirected_to(conn) == Routes.course_question_path(conn, :index, course_id)
    end
  end

  describe "unfollow question" do
    setup [:create_question]
    setup [:create_follow]

    test "unfollows question", %{conn: conn, course_id: course_id, question_id: question_id} do
      conn = delete(conn, Routes.course_question_follow_path(conn, :unfollow, course_id, question_id))
      assert redirected_to(conn) == Routes.course_question_path(conn, :index, course_id)
    end
  end

  
  defp create_follow(%{question_id: question_id, user_id: user_id}) do
    follow = fixture(:follow, question_id, user_id)
    %{follow: follow}
  end

  defp create_question(%{conn: conn}) do
    _conn_and_ids = fixture(:question, conn)
  end


  defp create_valid_attrs(question_id, user_id) do
    %{question_id: question_id, user_id: user_id}
    |> Morphix.atomorphify!()
  end
end
