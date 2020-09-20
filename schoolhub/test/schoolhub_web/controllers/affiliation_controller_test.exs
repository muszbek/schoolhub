defmodule SchoolhubWeb.AffiliationControllerTest do
  use SchoolhubWeb.ConnCase

  alias Schoolhub.Courses
  alias Schoolhub.Accounts

  @create_attrs %{affiliation: "student"}
  @update_attrs %{affiliation: "assistant"}
  @invalid_attrs %{affiliation: "some invalid affiliation"}

  @create_user_attrs %{email: "some email",
		       name: "some name",
		       credential: %{username: "some username",
				     password: "some password"}}
  @create_course_attrs %{description: "some description", name: "some name"}

  def fixture(:affiliation, course_id, user_id) do
    attrs = create_valid_attrs(@create_attrs, course_id, user_id)
    {:ok, affiliation} = Courses.create_affiliation(attrs)
    affiliation
  end

  def fixture(:course, conn = %Plug.Conn{}) do
    {:ok, user = %{id: user_id}} = Accounts.create_user(@create_user_attrs)
    
    new_conn = conn
    |> Plug.Test.init_test_session(user_id: nil)
    |> SchoolhubWeb.SessionController.enter_session(user)

    {:ok, _course = %{id: course_id}} = Courses.create_course(@create_course_attrs)
    
    %{conn: new_conn, course_id: course_id, user_id: user_id}
  end
  

  describe "index" do
    setup [:create_course]
    
    test "lists all course_affiliations", %{conn: conn, course_id: course_id} do
      conn = get(conn, Routes.course_affiliation_path(conn, :index, course_id))
      assert html_response(conn, 200) =~ "Listing Course affiliations"
    end
  end

  describe "new affiliation" do
    setup [:create_course]
    
    test "renders form", %{conn: conn, course_id: course_id} do
      conn = get(conn, Routes.course_affiliation_path(conn, :new, course_id))
      assert html_response(conn, 200) =~ "New Affiliation"
    end
  end

  describe "create affiliation" do
    setup [:create_course]
    
    test "redirects to show when data is valid", %{conn: conn,
						   course_id: course_id, user_id: user_id} do
      attrs = create_valid_attrs(@create_attrs, course_id, user_id)
      conn = post(conn, Routes.course_affiliation_path(conn, :create, course_id),
	affiliation: attrs)

      assert %{id: id} = redirected_params(conn)
      assert redirected_to(conn) == Routes.course_affiliation_path(conn, :show, course_id, id)

      conn = get(conn, Routes.course_affiliation_path(conn, :show, course_id, id))
      assert html_response(conn, 200) =~ "Show Affiliation"
    end

    test "renders errors when data is invalid", %{conn: conn,
						  course_id: course_id, user_id: user_id} do
      invalid_attrs = create_valid_attrs(@invalid_attrs, course_id, user_id)
      conn = post(conn, Routes.course_affiliation_path(conn, :create, course_id),
	affiliation: invalid_attrs)
      assert html_response(conn, 200) =~ "New Affiliation"
    end
  end

  describe "edit affiliation" do
    setup [:create_course]
    setup [:create_affiliation]

    test "renders form for editing chosen affiliation", %{conn: conn, affiliation: affiliation,
							  course_id: course_id} do
      conn = get(conn, Routes.course_affiliation_path(conn, :edit, course_id, affiliation))
      assert html_response(conn, 200) =~ "Edit Affiliation"
    end
  end

  describe "update affiliation" do
    setup [:create_course]
    setup [:create_affiliation]

    test "redirects when data is valid", %{conn: conn, affiliation: affiliation,
					   course_id: course_id} do
      conn = put(conn, Routes.course_affiliation_path(conn, :update, course_id, affiliation),
	affiliation: @update_attrs)
      assert redirected_to(conn) == Routes.course_affiliation_path(conn, :show, course_id, affiliation)

      conn = get(conn, Routes.course_affiliation_path(conn, :show, course_id, affiliation))
      assert html_response(conn, 200) =~ "assistant"
    end

    test "renders errors when data is invalid", %{conn: conn, affiliation: affiliation,
						  course_id: course_id} do
      conn = put(conn, Routes.course_affiliation_path(conn, :update, course_id, affiliation),
	affiliation: @invalid_attrs)
      assert html_response(conn, 200) =~ "Edit Affiliation"
    end
  end

  describe "delete affiliation" do
    setup [:create_course]
    setup [:create_affiliation]

    test "deletes chosen affiliation", %{conn: conn, affiliation: affiliation,
					 course_id: course_id} do
      conn = delete(conn, Routes.course_affiliation_path(conn, :delete, course_id, affiliation))
      assert redirected_to(conn) == Routes.course_affiliation_path(conn, :index, course_id)
      assert_error_sent 404, fn ->
        get(conn, Routes.course_affiliation_path(conn, :show, course_id, affiliation))
      end
    end
  end

  
  defp create_affiliation(%{course_id: course_id, user_id: user_id}) do
    affiliation = fixture(:affiliation, course_id, user_id)
    %{affiliation: affiliation}
  end

  defp create_course(%{conn: conn}) do
    _conn_and_ids = fixture(:course, conn)
  end


  defp create_valid_attrs(attrs, course_id, user_id) do
    attrs
    |> Map.put("course_id", course_id)
    |> Map.put("user_id", user_id)
    |> Morphix.atomorphify!()
  end

end
