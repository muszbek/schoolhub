defmodule SchoolhubWeb.QreplyControllerTest do
  use SchoolhubWeb.ConnCase

  alias Schoolhub.{Accounts, Courses, Privileges, Questions}

  @create_attrs %{content: "some content"}
  @update_attrs %{content: "some updated content"}
  @invalid_attrs %{content: nil}

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

    %{conn: new_conn, course_id: course_id, parent_question: question.id, creator: user.id}
  end
  
  def fixture(:qreply, question_id, creator) do
    reply_attrs = create_valid_attrs(@create_attrs, question_id, creator)
    {:ok, reply} = Questions.create_qreply(reply_attrs)
    reply
  end
  

  describe "index" do
    setup [:create_question]
    
    test "lists all question_replies", %{conn: conn, course_id: course_id,
					 parent_question: question_id} do
      conn = get(conn, Routes.course_question_qreply_path(conn, :index, course_id, question_id))
      assert redirected_to(conn) == Routes.course_question_path(conn, :show,
	course_id, question_id)
    end
  end

  describe "new reply" do
    setup [:create_question]
    
    test "renders form", %{conn: conn, course_id: course_id,
			   parent_question: question_id} do
      conn = get(conn, Routes.course_question_qreply_path(conn, :new, course_id, question_id))
      assert html_response(conn, 200) =~ "New Reply"
    end
  end

  describe "create qreply" do
    setup [:create_question]
    
    test "redirects to show when data is valid", %{conn: conn, course_id: course_id,
						   parent_question: question_id} do
      conn = post(conn, Routes.course_question_qreply_path(conn, :create, course_id, question_id),
	qreply: @create_attrs)

      assert %{id: id} = redirected_params(conn)
      assert redirected_to(conn) == Routes.course_question_qreply_path(conn, :show,
	course_id, question_id, id)

      conn = get(conn, Routes.course_question_qreply_path(conn, :show, course_id, question_id, id))
      assert html_response(conn, 200) =~ "Show Reply"
    end

    test "renders errors when data is invalid", %{conn: conn, course_id: course_id,
						  parent_question: question_id} do
      conn = post(conn, Routes.course_question_qreply_path(conn, :create, course_id, question_id),
	qreply: @invalid_attrs)
      assert html_response(conn, 200) =~ "New Reply"
    end
  end

  describe "edit qreply" do
    setup [:create_question]
    setup [:create_qreply]

    test "renders form for editing chosen qreply", %{conn: conn, course_id: course_id,
						     parent_question: question_id, qreply: qreply} do
      conn = get(conn, Routes.course_question_qreply_path(conn, :edit,
	    course_id, question_id, qreply))
      assert html_response(conn, 200) =~ "Edit Reply"
    end
  end

  describe "update qreply" do
    setup [:create_question]
    setup [:create_qreply]

    test "redirects when data is valid", %{conn: conn, course_id: course_id,
					   parent_question: question_id, qreply: qreply} do
      conn = put(conn, Routes.course_question_qreply_path(conn, :update,
	    course_id, question_id, qreply), qreply: @update_attrs)
      assert redirected_to(conn) == Routes.course_question_qreply_path(conn, :show,
	course_id, question_id, qreply)

      conn = get(conn, Routes.course_question_qreply_path(conn, :show,
	    course_id, question_id, qreply))
      assert html_response(conn, 200) =~ "some updated content"
    end

    test "renders errors when data is invalid", %{conn: conn, course_id: course_id,
						  parent_question: question_id, qreply: qreply} do
      conn = put(conn, Routes.course_question_qreply_path(conn, :update,
	    course_id, question_id, qreply), qreply: @invalid_attrs)
      assert html_response(conn, 200) =~ "Edit Reply"
    end
  end

  describe "delete qreply" do
    setup [:create_question]
    setup [:create_qreply]

    test "deletes chosen qreply", %{conn: conn, course_id: course_id,
				    parent_question: question_id, qreply: qreply} do
      conn = delete(conn, Routes.course_question_qreply_path(conn, :delete,
	    course_id, question_id, qreply))
      assert redirected_to(conn) == Routes.course_question_qreply_path(conn, :index,
	course_id, question_id)
      assert_error_sent 404, fn ->
        get(conn, Routes.course_question_qreply_path(conn, :show, course_id, question_id, qreply))
      end
    end
  end

  
  defp create_qreply(%{parent_question: question_id, creator: creator}) do
    qreply = fixture(:qreply, question_id, creator)
    %{qreply: qreply}
  end

  defp create_question(%{conn: conn}) do
    _conn_and_ids = fixture(:question, conn)
  end


  defp create_valid_attrs(attrs, question_id, creator) do
    attrs
    |> Map.put("parent_question", question_id)
    |> Map.put("creator", creator)
    |> Morphix.atomorphify!()
  end
end
