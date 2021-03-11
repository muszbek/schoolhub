defmodule SchoolhubWeb.QuestionControllerTest do
  use SchoolhubWeb.ConnCase

  alias Schoolhub.{Accounts, Courses, Privileges, Questions}

  @create_attrs %{content: "some content", pinned: true, tags: "some_tag"}
  @update_attrs %{content: "some updated content", pinned: false, tags: ""}
  @invalid_attrs %{content: nil, pinned: nil, tags: ""}

  @create_user_attrs %{email: "some email",
		       name: "some name",
		       credential: %{username: "some username",
				     password: "some password"}}
  @create_course_attrs %{description: "some description", name: "some name"}

  def fixture(:course, conn = %Plug.Conn{}) do
    {:ok, user} = Accounts.create_user(@create_user_attrs)
    Privileges.update_privilege(user.privilege, %{level: "admin"})
    
    new_conn = conn
    |> Plug.Test.init_test_session(user_id: nil)
    |> SchoolhubWeb.SessionController.enter_session(user)

    {:ok, _course = %{id: course_id}} = Courses.create_course(@create_course_attrs)
    aff_attrs = %{course_id: course_id, user_id: user.id, affiliation: "owner"}
    {:ok, _aff} = Courses.create_affiliation(aff_attrs)

    %{conn: new_conn, course_id: course_id, creator: user.id}
  end

  def fixture(:question, course_id, creator) do
    question_attrs = @create_attrs
    |>create_valid_attrs(course_id, creator)
    |> Map.update(:tags, [], &(String.split(&1, " ", [trim: true])))
    
    {:ok, question} = Questions.create_question(question_attrs)
    question
  end
  

  describe "index" do
    setup [:create_course]
    
    test "lists all questions", %{conn: conn, course_id: course_id} do
      conn = get(conn, Routes.course_question_path(conn, :index, course_id))
      assert html_response(conn, 200) =~ "Listing Questions"
    end
  end

  describe "filter questions" do
    setup [:create_course]

    test "lists all questions", %{conn: conn, course_id: course_id} do
      conn = post(conn, Routes.course_question_path(conn, :filter, course_id, filters: ""))
      assert html_response(conn, 200) =~ "Listing Questions"
    end

    test "lists no questions", %{conn: conn, course_id: course_id} do
      conn = post(conn, Routes.course_question_path(conn, :filter, course_id, filters: "invalid filters"))
      assert html_response(conn, 200) =~ "Listing Questions"
    end
  end

  describe "new question" do
    setup [:create_course]
    
    test "renders form", %{conn: conn, course_id: course_id} do
      conn = get(conn, Routes.course_question_path(conn, :new, course_id))
      assert html_response(conn, 200) =~ "New Question"
    end
  end

  describe "create question" do
    setup [:create_course]
    
    test "redirects to show when data is valid", %{conn: conn, course_id: course_id} do
      creator = get_session(conn, :user_id)
      question_attrs = create_valid_attrs(@create_attrs, course_id, creator)
      conn = post(conn, Routes.course_question_path(conn, :create, course_id), question: question_attrs)

      assert %{id: id} = redirected_params(conn)
      assert redirected_to(conn) == Routes.course_question_path(conn, :show, course_id, id)

      conn = get(conn, Routes.course_question_path(conn, :show, course_id, id))
      assert html_response(conn, 200) =~ "Show Question"
    end

    test "renders errors when data is invalid", %{conn: conn, course_id: course_id} do
      conn = post(conn, Routes.course_question_path(conn, :create, course_id), question: @invalid_attrs)
      assert html_response(conn, 200) =~ "New Question"
    end
  end

  describe "edit question" do
    setup [:create_course]
    setup [:create_question]

    test "renders form for editing chosen question", %{conn: conn, course_id: course_id, question: question} do
      conn = get(conn, Routes.course_question_path(conn, :edit, course_id, question))
      assert html_response(conn, 200) =~ "Edit Question"
    end
  end

  describe "update question" do
    setup [:create_course]
    setup [:create_question]

    test "redirects when data is valid", %{conn: conn, course_id: course_id, question: question} do
      conn = put(conn, Routes.course_question_path(conn, :update, course_id, question), question: @update_attrs)
      assert redirected_to(conn) == Routes.course_question_path(conn, :show, course_id, question)

      conn = get(conn, Routes.course_question_path(conn, :show, course_id, question))
      assert html_response(conn, 200) =~ "some updated content"
    end

    test "renders errors when data is invalid", %{conn: conn, course_id: course_id, question: question} do
      conn = put(conn, Routes.course_question_path(conn, :update, course_id, question), question: @invalid_attrs)
      assert html_response(conn, 200) =~ "Edit Question"
    end
  end

  describe "delete question" do
    setup [:create_course]
    setup [:create_question]

    test "deletes chosen question", %{conn: conn, course_id: course_id, question: question} do
      conn = delete(conn, Routes.course_question_path(conn, :delete, course_id, question))
      assert redirected_to(conn) == Routes.course_question_path(conn, :index, course_id)
      assert_error_sent 404, fn ->
        get(conn, Routes.course_question_path(conn, :show, course_id, question))
      end
    end
  end

  describe "pin question" do
    setup [:create_course]
    setup [:create_question]

    test "pins question", %{conn: conn, course_id: course_id, question: question} do
      conn = put(conn, Routes.course_question_question_path(conn, :pin, course_id, question), to_pin: true)
      assert redirected_to(conn) == Routes.course_question_path(conn, :index, course_id)

      conn = get(conn, Routes.course_question_path(conn, :show, course_id, question))
      assert html_response(conn, 200) =~ "true"
    end

    test "unpins question", %{conn: conn, course_id: course_id, question: question} do
      conn = put(conn, Routes.course_question_question_path(conn, :pin, course_id, question), to_pin: false)
      assert redirected_to(conn) == Routes.course_question_path(conn, :index, course_id)

      conn = get(conn, Routes.course_question_path(conn, :show, course_id, question))
      assert html_response(conn, 200) =~ "false"
    end
  end

  
  defp create_question(%{course_id: course_id, creator: creator}) do
    question = fixture(:question, course_id, creator)
    %{question: question}
  end

  defp create_course(%{conn: conn}) do
    _conn_and_ids = fixture(:course, conn)
  end


  defp create_valid_attrs(attrs, course_id, creator) do
    attrs
    |> Map.put("course_id", course_id)
    |> Map.put("creator", creator)
    |> Morphix.atomorphify!()
  end
end
