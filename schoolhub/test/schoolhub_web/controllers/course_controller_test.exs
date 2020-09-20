defmodule SchoolhubWeb.CourseControllerTest do
  use SchoolhubWeb.ConnCase

  alias Schoolhub.Courses
  alias Schoolhub.Accounts

  @create_attrs %{description: "some description", name: "some name"}
  @update_attrs %{description: "some updated description", name: "some updated name"}
  @invalid_attrs %{description: nil, name: nil}

  @create_user_attrs %{email: "some email",
		       name: "some name",
		       credential: %{username: "some username",
				     password: "some password"}}

  def fixture(:course) do
    {:ok, course} = Courses.create_course(@create_attrs)
    course
  end

  def fixture(:session, conn = %Plug.Conn{}) do
    {:ok, user} = Accounts.create_user(@create_user_attrs)
    
    conn
    |> Plug.Test.init_test_session(user_id: nil)
    |> SchoolhubWeb.SessionController.enter_session(user)
  end

  
  describe "index" do
    setup [:enter_session]
    
    test "lists all courses", %{conn: conn} do
      conn = get(conn, Routes.course_path(conn, :index))
      assert html_response(conn, 200) =~ "Listing Courses"
    end
  end

  describe "new course" do
    setup [:enter_session]
    
    test "renders form", %{conn: conn} do
      conn = get(conn, Routes.course_path(conn, :new))
      assert html_response(conn, 200) =~ "New Course"
    end
  end

  describe "create course" do
    setup [:enter_session]
    
    test "redirects to show when data is valid", %{conn: conn} do
      conn = post(conn, Routes.course_path(conn, :create), course: @create_attrs)

      assert %{id: id} = redirected_params(conn)
      assert redirected_to(conn) == Routes.course_path(conn, :show, id)

      conn = get(conn, Routes.course_path(conn, :show, id))
      assert html_response(conn, 200) =~ "Show Course"
    end

    test "renders errors when data is invalid", %{conn: conn} do
      conn = post(conn, Routes.course_path(conn, :create), course: @invalid_attrs)
      assert html_response(conn, 200) =~ "New Course"
    end
  end

  describe "edit course" do
    setup [:enter_session]
    setup [:create_course]

    test "renders form for editing chosen course", %{conn: conn, course: course} do
      conn = get(conn, Routes.course_path(conn, :edit, course))
      assert html_response(conn, 200) =~ "Edit Course"
    end
  end

  describe "update course" do
    setup [:enter_session]
    setup [:create_course]

    test "redirects when data is valid", %{conn: conn, course: course} do
      conn = put(conn, Routes.course_path(conn, :update, course), course: @update_attrs)
      assert redirected_to(conn) == Routes.course_path(conn, :show, course)

      conn = get(conn, Routes.course_path(conn, :show, course))
      assert html_response(conn, 200) =~ "some updated description"
    end

    test "renders errors when data is invalid", %{conn: conn, course: course} do
      conn = put(conn, Routes.course_path(conn, :update, course), course: @invalid_attrs)
      assert html_response(conn, 200) =~ "Edit Course"
    end
  end

  describe "delete course" do
    setup [:enter_session]
    setup [:create_course]

    test "deletes chosen course", %{conn: conn, course: course} do
      conn = delete(conn, Routes.course_path(conn, :delete, course))
      assert redirected_to(conn) == Routes.course_path(conn, :index)
      assert_error_sent 404, fn ->
        get(conn, Routes.course_path(conn, :show, course))
      end
    end
  end

  
  defp create_course(_) do
    course = fixture(:course)
    %{course: course}
  end

  defp enter_session(%{conn: conn}) do
    new_conn = fixture(:session, conn)
    %{conn: new_conn}
  end
end