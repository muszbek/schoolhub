defmodule SchoolhubWeb.AffiliationControllerTest do
  use SchoolhubWeb.ConnCase
  
  alias Schoolhub.{Courses, Accounts, Privileges}

  @create_attrs %{affiliation: "student"}
  @update_attrs %{affiliation: "assistant"}
  @owner_attrs %{affiliation: "owner"}
  @invalid_attrs %{affiliation: "some invalid affiliation"}

  @create_user_attrs %{email: "some email",
		       name: "some name",
		       credential: %{username: "some username",
				     password: "some password"}}
  @other_user_attrs %{email: "some other email",
		      name: "some other name",
		      credential: %{username: "some other username",
				    password: "some other password"}}
  @create_course_attrs %{description: "some description", name: "some name"}

  def fixture(:course, conn = %Plug.Conn{}) do
    {:ok, user} = Accounts.create_user(@create_user_attrs)
    Privileges.update_privilege(user.privilege, %{level: "admin"})
    
    new_conn = conn
    |> Plug.Test.init_test_session(user_id: nil)
    |> SchoolhubWeb.SessionController.enter_session(user)

    {:ok, _course = %{id: course_id}} = Courses.create_course(@create_course_attrs)
    aff_attrs = create_valid_attrs(@owner_attrs, course_id, user.id)
    {:ok, affiliation} = Courses.create_affiliation(aff_attrs)

    {:ok, _other_user = %{id: user_id, credential: %{username: username}}} =
      Accounts.create_user(@other_user_attrs)

    %{conn: new_conn, course_id: course_id, affiliation: affiliation,
      other_user_id: user_id, other_username: username}
  end

  def fixture(:other_affiliation, course_id, user_id) do
    aff_attrs = create_valid_attrs(@create_attrs, course_id, user_id)
    {:ok, affiliation} = Courses.create_affiliation(aff_attrs)
    affiliation
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
      assert html_response(conn, 200) =~ "Add Member"
    end
  end

  describe "create affiliation" do
    setup [:create_course]
    
    test "redirects to show when data is valid", %{conn: conn, course_id: course_id,
						   other_username: username} do
      attrs = create_valid_attrs(@create_attrs, course_id, username)
      ## controller needs username as user_id
      conn = post(conn, Routes.course_affiliation_path(conn, :create, course_id),
	affiliation: attrs)

      assert %{id: id} = redirected_params(conn)
      assert redirected_to(conn) == Routes.course_affiliation_path(conn, :show, course_id, id)

      conn = get(conn, Routes.course_affiliation_path(conn, :show, course_id, id))
      assert html_response(conn, 200) =~ "Show Affiliation"
    end

    test "renders errors when data is invalid", %{conn: conn, course_id: course_id,
						  other_username: username} do
      invalid_attrs = create_valid_attrs(@invalid_attrs, course_id, username)
      ## controller needs username as user_id
      conn = post(conn, Routes.course_affiliation_path(conn, :create, course_id),
	affiliation: invalid_attrs)
      assert html_response(conn, 200) =~ "Add Member"
    end
  end

  describe "edit affiliation" do
    setup [:create_course]

    test "renders form for editing chosen affiliation", %{conn: conn, affiliation: affiliation,
							  course_id: course_id} do
      conn = get(conn, Routes.course_affiliation_path(conn, :edit, course_id, affiliation))
      assert html_response(conn, 200) =~ "Change Affiliation"
    end
  end

  describe "update affiliation" do
    setup [:create_course]
    setup [:create_affiliation]

    test "redirects when data is valid", %{conn: conn, other_affiliation: affiliation,
					   course_id: course_id} do
      conn = put(conn, Routes.course_affiliation_path(conn, :update, course_id, affiliation),
	affiliation: @update_attrs)
      assert redirected_to(conn) == Routes.course_affiliation_path(conn, :show, course_id, affiliation)

      conn = get(conn, Routes.course_affiliation_path(conn, :show, course_id, affiliation))
      assert html_response(conn, 200) =~ "assistant"
    end

    test "renders errors when data is invalid", %{conn: conn, other_affiliation: affiliation,
						  course_id: course_id} do
      conn = put(conn, Routes.course_affiliation_path(conn, :update, course_id, affiliation),
	affiliation: @invalid_attrs)
      assert html_response(conn, 200) =~ "Change Affiliation"
    end

    test "change to owner valid when there is none", %{conn: conn, affiliation: affiliation,
						       course_id: course_id} do

      Courses.update_affiliation(affiliation, @create_attrs)
      
      conn = put(conn, Routes.course_affiliation_path(conn, :update, course_id, affiliation),
	affiliation: @owner_attrs)
      assert redirected_to(conn) == Routes.course_affiliation_path(conn, :show, course_id, affiliation)

      conn = get(conn, Routes.course_affiliation_path(conn, :show, course_id, affiliation))
      assert html_response(conn, 200) =~ "owner"
    end
  end
  
  describe "set owner" do
    setup [:create_course]
    setup [:create_affiliation]
    
    test "cannot change self if owner", %{conn: conn, affiliation: affiliation,
					  course_id: course_id} do
      
      conn = put(conn, Routes.course_affiliation_path(conn, :update, course_id, affiliation),
	affiliation: @update_attrs)
      assert redirected_to(conn) == Routes.course_affiliation_path(conn, :show, course_id, affiliation)

      conn = get(conn, Routes.course_affiliation_path(conn, :show, course_id, affiliation))
      assert html_response(conn, 200) =~ "owner"
    end

    test "change owner to owner nothing happens", %{conn: conn, affiliation: affiliation,
						    course_id: course_id} do
      
      conn = put(conn, Routes.course_affiliation_path(conn, :update, course_id, affiliation),
	affiliation: @owner_attrs)
      assert redirected_to(conn) == Routes.course_affiliation_path(conn, :show, course_id, affiliation)

      conn = get(conn, Routes.course_affiliation_path(conn, :show, course_id, affiliation))
      assert html_response(conn, 200) =~ "owner"
    end

    test "change other user owner demotes previous", %{conn: conn, affiliation: affiliation,
						       other_affiliation: other_aff,
						       course_id: course_id} do
      
      conn = put(conn, Routes.course_affiliation_path(conn, :update, course_id, other_aff),
	affiliation: @owner_attrs)
      assert redirected_to(conn) == Routes.course_affiliation_path(conn, :show, course_id, other_aff)

      conn = get(conn, Routes.course_affiliation_path(conn, :show, course_id, other_aff))
      assert html_response(conn, 200) =~ "owner"

      conn = get(conn, Routes.course_affiliation_path(conn, :show, course_id, affiliation))
      assert html_response(conn, 200) =~ "assistant"
    end
  end

  describe "delete affiliation" do
    setup [:create_course]
    setup [:create_affiliation]

    test "deletes chosen affiliation", %{conn: conn, course_id: course_id,
					 other_affiliation: affiliation} do
      
      conn = delete(conn, Routes.course_affiliation_path(conn, :delete, course_id, affiliation))
      assert redirected_to(conn) == Routes.course_affiliation_path(conn, :index, course_id)
      assert_error_sent 404, fn ->
        get(conn, Routes.course_affiliation_path(conn, :show, course_id, affiliation))
      end
    end

    test "cannot delete owner", %{conn: conn, affiliation: affiliation,
				  course_id: course_id} do
      
      conn = delete(conn, Routes.course_affiliation_path(conn, :delete, course_id, affiliation))
      assert redirected_to(conn) == Routes.course_affiliation_path(conn, :index, course_id)

      conn = get(conn, Routes.course_affiliation_path(conn, :show, course_id, affiliation))
      assert html_response(conn, 200) =~ "owner"
    end
  end


  defp create_course(%{conn: conn}) do
    _conn_and_ids = fixture(:course, conn)
  end

  defp create_affiliation(%{course_id: course_id, other_user_id: user_id}) do
    aff = fixture(:other_affiliation, course_id, user_id)
    %{other_affiliation: aff}
  end


  defp create_valid_attrs(attrs, course_id, username) do
    attrs
    |> Map.put("course_id", course_id)
    |> Map.put("user_id", username)
    |> Morphix.atomorphify!()
  end

end
