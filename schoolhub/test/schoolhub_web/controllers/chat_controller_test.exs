defmodule SchoolhubWeb.ChatControllerTest do
  use SchoolhubWeb.ConnCase

  alias Schoolhub.{Accounts, Privileges, Courses}

  @create_user_attrs %{email: "some email",
		       name: "some name",
		       credential: %{username: "some username",
				     password: "some password"}}
  @create_course_attrs %{description: "some description", name: "some name"}
  @owner_attrs %{affiliation: "owner"}
  
  def fixture(:course, conn = %Plug.Conn{}) do
    {:ok, user} = Accounts.create_user(@create_user_attrs)
    Privileges.update_privilege(user.privilege, %{level: "admin"})
    
    new_conn = conn
    |> Plug.Test.init_test_session(user_id: nil)
    |> SchoolhubWeb.SessionController.enter_session(user)

    {:ok, _course = %{id: course_id}} = Courses.create_course(@create_course_attrs)
    aff_attrs = @owner_attrs
    |> Map.put(:course_id, course_id)
    |> Map.put(:user_id, user.id)
    
    {:ok, _aff} = Courses.create_affiliation(aff_attrs)

    %{conn: new_conn, course_id: course_id, user_id: user.id}
  end


  describe "index" do
    setup [:create_course]

    test "list affiliated users", %{conn: conn, course_id: course_id} do
      conn = get(conn, Routes.course_chat_path(conn, :index, course_id))
      assert html_response(conn, 200) =~ "Listing Users"
    end
  end

  describe "chat" do
    setup [:create_course]

    test "show chat page", %{conn: conn, course_id: course_id, user_id: user_id} do
      conn = add_tokens(conn)
      conn = get(conn, Routes.course_chat_path(conn, :chat, course_id, user_id))
      assert html_response(conn, 200) =~ "Chat"
    end

    test "no token redirect", %{conn: conn, course_id: course_id, user_id: user_id} do
      conn = get(conn, Routes.course_chat_path(conn, :chat, course_id, user_id))
      assert redirected_to(conn) == Routes.session_path(conn, :new)
    end
  end

  
  defp create_course(%{conn: conn}) do
    _course_id = fixture(:course, conn)
  end

  defp add_tokens(conn) do
    conn
    |> put_session(:access_token, "some access token")
    |> put_session(:refresh_token, "some refresh token")
  end
end
