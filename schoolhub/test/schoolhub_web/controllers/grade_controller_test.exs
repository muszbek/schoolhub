defmodule SchoolhubWeb.GradeControllerTest do
  use SchoolhubWeb.ConnCase

  alias Schoolhub.{Accounts, Privileges, Courses}

  @create_attrs %{title: "some grade title", grade: "some grade"}
  @update_attrs %{grades: "{}"}
  @invalid_attrs %{grades: ""}

  @create_user_attrs %{email: "some email",
		       name: "some name",
		       credential: %{username: "some username",
				     password: "some password"}}
  @create_course_attrs %{description: "some description", name: "some name"}
  @owner_attrs %{affiliation: "owner"}

  def fixture(:affiliation, conn = %Plug.Conn{}) do
    {:ok, user} = Accounts.create_user(@create_user_attrs)
    Privileges.update_privilege(user.privilege, %{level: "admin"})
    
    new_conn = conn
    |> Plug.Test.init_test_session(user_id: nil)
    |> SchoolhubWeb.SessionController.enter_session(user)

    {:ok, _course = %{id: course_id}} = Courses.create_course(@create_course_attrs)
    aff_attrs = @owner_attrs
    |> Map.put(:course_id, course_id)
    |> Map.put(:user_id, user.id)
    
    {:ok, affiliation} = Courses.create_affiliation(aff_attrs)
    grade = affiliation.grade

    %{conn: new_conn, course_id: course_id, affiliation_id: affiliation.id, grade: grade}
  end

  
  describe "index" do
    setup [:create_affiliation]
    
    test "lists all grades", %{conn: conn} do
      conn = get(conn, Routes.course_affiliation_grade_path(conn, :index))
      assert html_response(conn, 200) =~ "Listing Grades"
    end
  end

  describe "new grade" do
    setup [:create_affiliation]
    
    test "renders form", %{conn: conn, course_id: course_id, affiliation_id: aff_id} do
      conn = get(conn, Routes.course_affiliation_grade_path(conn, :new, course_id, aff_id))
      assert html_response(conn, 200) =~ "New Grade"
    end
  end

  describe "create grade" do
    setup [:create_affiliation]
    
    test "redirects to show when data is valid",
      %{conn: conn, course_id: course_id, affiliation_id: aff_id} do
      
      conn = post(conn, Routes.course_affiliation_grade_path(conn, :create, course_id, aff_id),
	@create_attrs)

      assert %{id: id} = redirected_params(conn)
      assert redirected_to(conn) == Routes.course_affiliation_grade_path(conn, :show, course_id, aff_id, id)

      conn = get(conn, Routes.course_affiliation_grade_path(conn, :show, course_id, aff_id, id))
      assert html_response(conn, 200) =~ "Show Grade"
    end
  end

  describe "edit grade" do
    setup [:create_affiliation]

    test "renders form for editing chosen grade",
      %{conn: conn, course_id: course_id, affiliation_id: aff_id, grade: grade} do
      
      conn = get(conn, Routes.course_affiliation_grade_path(conn, :edit, course_id, aff_id, grade))
      assert html_response(conn, 200) =~ "Edit Grade"
    end
  end

  describe "update grade" do
    setup [:create_affiliation]

    test "redirects when data is valid",
      %{conn: conn, course_id: course_id, affiliation_id: aff_id, grade: grade} do
      
      conn = put(conn, Routes.course_affiliation_grade_path(conn, :update, course_id, aff_id, grade), grade: @update_attrs)
      assert redirected_to(conn) == Routes.course_affiliation_grade_path(conn, :show, course_id, aff_id, grade)

      conn = get(conn, Routes.course_affiliation_grade_path(conn, :show, course_id, aff_id, grade))
      assert html_response(conn, 200)
    end

    test "renders errors when data is invalid",
      %{conn: conn, course_id: course_id, affiliation_id: aff_id, grade: grade} do
      
      conn = put(conn, Routes.course_affiliation_grade_path(conn, :update, course_id, aff_id, grade), grade: @invalid_attrs)
      assert html_response(conn, 200) =~ "Edit Grade"
    end
  end

  describe "delete grade" do
    setup [:create_affiliation]

    test "deletes chosen grade", %{conn: conn, grade: grade} do
      conn = delete(conn, Routes.course_affiliation_grade_path(conn, :delete, grade))
      assert redirected_to(conn) == Routes.course_affiliation_grade_path(conn, :index)
      assert_error_sent 404, fn ->
        get(conn, Routes.course_affiliation_grade_path(conn, :show, grade))
      end
    end
  end

  
  defp create_affiliation(%{conn: conn}) do
    _aff_id = fixture(:affiliation, conn)
  end
  
end
