defmodule SchoolhubWeb.TokenControllerTest do
  use SchoolhubWeb.ConnCase

  alias Schoolhub.{Accounts, Courses, Privileges}

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
    
    %{conn: new_conn, course_id: course_id, user_id: user.id}
  end

  def fixture(:affiliation, course_id, user_id) do
    aff_attrs = %{course_id: course_id, user_id: user_id, affiliation: "student"}
    {:ok, aff} = Courses.create_affiliation(aff_attrs)
    %{affiliation: aff}
  end


  describe "join with token page" do
    setup [:create_course]
    
    test "renders form", %{conn: conn} do
      conn = get(conn, Routes.token_path(conn, :new))
      assert html_response(conn, 200) =~ "token"
    end
  end

  describe "join course without membership" do
    setup [:create_course]
    
    test "redirects to course when token is valid", %{conn: conn, course_id: course_id} do
      token = Courses.create_token(course_id)
      conn = post(conn, Routes.token_path(conn, :create), token: token)
      assert redirected_to(conn) == Routes.course_path(conn, :show, course_id)
    end

    test "renders errors when token is invalid", %{conn: conn} do
      conn = post(conn, Routes.token_path(conn, :create), token: "invalid_token")
      assert redirected_to(conn) == Routes.token_path(conn, :new)
    end
  end

  describe "join course with membership" do
    setup [:create_course]
    setup [:create_affiliation]
    
    test "redirects to course when token is valid", %{conn: conn, course_id: course_id} do
      token = Courses.create_token(course_id)
      conn = post(conn, Routes.token_path(conn, :create), token: token)
      assert redirected_to(conn) == Routes.course_path(conn, :show, course_id)
    end

    test "renders errors when token is invalid", %{conn: conn} do
      conn = post(conn, Routes.token_path(conn, :create), token: "invalid_token")
      assert redirected_to(conn) == Routes.token_path(conn, :new)
    end
  end


  defp create_affiliation(%{course_id: course_id, user_id: user_id}) do
    _aff = fixture(:affiliation, course_id, user_id)
  end

  defp create_course(%{conn: conn}) do
    _conn_and_ids = fixture(:course, conn)
  end

end
