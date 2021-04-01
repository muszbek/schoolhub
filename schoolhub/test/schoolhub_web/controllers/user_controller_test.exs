defmodule SchoolhubWeb.UserControllerTest do
  use SchoolhubWeb.ConnCase

  alias Schoolhub.{Accounts, Privileges}

  @create_attrs %{email: "some email",
		  name: "some name",
		  credential: %{username: "some username",
				password: "some password"}}
  @update_attrs %{email: "some updated email",
		  name: "some updated name",
		  credential: %{username: "some updated username",
				password: "some updated password"}}
  @invalid_attrs %{email: nil, name: nil}

  @admin_attrs %{level: "admin"}

  def fixture(:user) do
    {:ok, user} = Accounts.create_user(@create_attrs)
    Privileges.update_privilege(user.privilege, @admin_attrs)
    user
  end

  def fixture(:session, conn = %Plug.Conn{}, user) do
    conn
    |> Plug.Test.init_test_session(user_id: nil)
    |> SchoolhubWeb.SessionController.enter_session(user)
  end
  

  describe "index" do
    setup [:create_user]
    setup [:enter_session]
    
    test "lists all users", %{conn: conn} do
      conn = get(conn, Routes.user_path(conn, :index))
      assert html_response(conn, 200) =~ "Listing Users"
    end
  end

  describe "new user" do
    test "renders form", %{conn: conn} do
      conn = get(conn, Routes.user_path(conn, :new))
      assert html_response(conn, 200) =~ "New User"
    end
  end

  describe "create user" do
    test "redirects to show when data is valid", %{conn: conn} do
      conn = post(conn, Routes.user_path(conn, :create), user: @create_attrs)
      assert redirected_to(conn) == Routes.session_path(conn, :new)
    end

    test "renders errors when data is invalid", %{conn: conn} do
      conn = post(conn, Routes.user_path(conn, :create), user: @invalid_attrs)
      assert html_response(conn, 200) =~ "New User"
    end
  end

  describe "edit user" do
    setup [:create_user]
    setup [:enter_session]

    test "renders form for editing chosen user", %{conn: conn, user: user} do
      conn = get(conn, Routes.user_path(conn, :edit, user))
      assert html_response(conn, 200) =~ "Edit User"
    end
  end

  describe "update user" do
    setup [:create_user]
    setup [:enter_session]

    test "redirects when data is valid", %{conn: conn, user: user} do
      conn = put(conn, Routes.user_path(conn, :update, user), user: @update_attrs)
      assert redirected_to(conn) == Routes.user_path(conn, :show, user)

      conn = get(conn, Routes.user_path(conn, :show, user))
      assert html_response(conn, 200) =~ "some updated email"
    end

    test "renders errors when data is invalid", %{conn: conn, user: user} do
      conn = put(conn, Routes.user_path(conn, :update, user), user: @invalid_attrs)
      assert html_response(conn, 200) =~ "Edit User"
    end
  end

  describe "delete user" do
    setup [:create_user]

    test "deletes chosen user", %{conn: conn, user: user} do
      conn = delete(conn, Routes.user_path(conn, :delete, user))
      assert redirected_to(conn) == Routes.session_path(conn, :new)
      #assert_error_sent 404, fn ->
      #  get(conn, Routes.user_path(conn, :show, user))
      #end
    end
  end

  describe "change password" do
    setup [:create_user]

    test "correct token renders change pw page", %{conn: conn, user: user} do
      username = user.credential.username
      token = Accounts.create_token(username)
      conn = get(conn, Routes.user_path(conn, :change_pw, token))
      assert html_response(conn, 200) =~ username
    end

    test "invalid token redirects", %{conn: conn} do
      token = "invalid_token"
      conn = get(conn, Routes.user_path(conn, :change_pw, token))
      assert redirected_to(conn) == Routes.session_path(conn, :new)
    end
  end

  describe "update password" do
    setup [:create_user]

    test "correct token update redirects", %{conn: conn, user: user} do
      username = user.credential.username
      token = Accounts.create_token(username)
      new_pw = "some updated password"
      
      conn = put(conn, Routes.user_path(conn, :update_pw, token), password: new_pw)
      assert redirected_to(conn) == Routes.session_path(conn, :new)
    end

    test "invalid token update redirects", %{conn: conn} do
      token = "invalid_token"
      new_pw = "some updated password"

      conn = put(conn, Routes.user_path(conn, :update_pw, token), password: new_pw)
      assert redirected_to(conn) == Routes.session_path(conn, :new)
    end

    test "invalid password update redirects", %{conn: conn, user: user} do
      username = user.credential.username
      token = Accounts.create_token(username)
      new_pw = nil
      
      conn = put(conn, Routes.user_path(conn, :update_pw, token), password: new_pw)
      assert redirected_to(conn) == Routes.user_path(conn, :change_pw, token)
    end
  end
  

  defp create_user(_) do
    user = fixture(:user)
    %{user: user}
  end

  defp enter_session(%{conn: conn, user: user}) do
    new_conn = fixture(:session, conn, user)
    %{conn: new_conn}
  end
end
