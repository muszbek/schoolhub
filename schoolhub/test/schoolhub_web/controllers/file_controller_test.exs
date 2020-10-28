defmodule SchoolhubWeb.FileControllerTest do
  use SchoolhubWeb.ConnCase

  alias Schoolhub.{Accounts, Courses, Privileges, Files}

  @create_attrs %{data: "some data", filename: "some filename", size: 120.5}
  @update_attrs %{data: "some updated data", filename: "some updated filename", size: 456.7}
  @invalid_attrs %{data: nil, filename: nil, size: nil}

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

    %{conn: new_conn, course_id: course_id, uploader: user.id}
  end
  
  def fixture(:file, course_id, uploader) do
    file_attrs = create_valid_attrs(@create_attrs, course_id, uploader)
    {:ok, file} = Files.create_file(file_attrs)
    file
  end

  
  describe "index" do
    setup [:create_course]
    
    test "lists all files", %{conn: conn, course_id: course_id} do
      conn = get(conn, Routes.course_file_path(conn, :index, course_id))
      assert html_response(conn, 200) =~ "Listing Files"
    end
  end

  describe "new file" do
    setup [:create_course]
    
    test "renders form", %{conn: conn, course_id: course_id} do
      conn = get(conn, Routes.course_file_path(conn, :new, course_id))
      assert html_response(conn, 200) =~ "New File"
    end
  end

  describe "create file" do
    setup [:create_course]
    
    test "redirects to show when data is valid", %{conn: conn, course_id: course_id, uploader: uploader} do
      file_attrs = create_valid_attrs(@create_attrs, course_id, uploader)
      conn = post(conn, Routes.course_file_path(conn, :create, course_id), file: file_attrs)

      assert %{id: id} = redirected_params(conn)
      assert redirected_to(conn) == Routes.course_file_path(conn, :show, course_id, id)

      conn = get(conn, Routes.course_file_path(conn, :show, course_id, id))
      assert html_response(conn, 200) =~ "Show File"
    end

    test "renders errors when data is invalid", %{conn: conn, course_id: course_id} do
      conn = post(conn, Routes.course_file_path(conn, :create, course_id), file: @invalid_attrs)
      assert html_response(conn, 200) =~ "New File"
    end
  end

  describe "edit file" do
    setup [:create_course]
    setup [:create_file]

    test "renders form for editing chosen file", %{conn: conn, course_id: course_id, course_file: file} do
      conn = get(conn, Routes.course_file_path(conn, :edit, course_id, file))
      assert html_response(conn, 200) =~ "Edit File"
    end
  end

  describe "update file" do
    setup [:create_course]
    setup [:create_file]

    test "redirects when data is valid", %{conn: conn, course_id: course_id, course_file: file} do
      conn = put(conn, Routes.course_file_path(conn, :update, course_id, file), file: @update_attrs)
      assert redirected_to(conn) == Routes.course_file_path(conn, :show, course_id, file)

      conn = get(conn, Routes.course_file_path(conn, :show, course_id, file))
      assert html_response(conn, 200) =~ "some updated filename"
    end

    test "renders errors when data is invalid", %{conn: conn, course_id: course_id, course_file: file} do
      conn = put(conn, Routes.course_file_path(conn, :update, course_id, file), file: @invalid_attrs)
      assert html_response(conn, 200) =~ "Edit File"
    end
  end

  describe "delete file" do
    setup [:create_course]
    setup [:create_file]

    test "deletes chosen file", %{conn: conn, course_id: course_id, course_file: file} do
      conn = delete(conn, Routes.course_file_path(conn, :delete, course_id, file))
      assert redirected_to(conn) == Routes.course_file_path(conn, :index, course_id)
      assert_error_sent 404, fn ->
        get(conn, Routes.course_file_path(conn, :show, course_id, file))
      end
    end
  end

  
  defp create_file(%{course_id: course_id, uploader: uploader}) do
    file = fixture(:file, course_id, uploader)
    %{course_file: file}
  end
    
  defp create_course(%{conn: conn}) do
    _conn_and_ids = fixture(:course, conn)
  end


  defp create_valid_attrs(attrs, course_id, uploader) do
    attrs
    |> Map.put("course_id", course_id)
    |> Map.put("uploader", uploader)
    |> Morphix.atomorphify!()
  end
end
