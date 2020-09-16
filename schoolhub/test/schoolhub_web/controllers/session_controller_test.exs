defmodule SchoolhubWeb.SessionControllerTest do
  use SchoolhubWeb.ConnCase

  alias Schoolhub.Accounts

  @user_attrs %{email: "some email",
		name: "some name",
		credential: %{username: "some username",
			      password: "some password"}}
  @auth_success %{username: "some username",
		  result: "authenticated"}
  @auth_fail %{username: "some username",
	       result: "some error"}
  @auth_client_first %{data: "n,,n=some username,r=80151d1f366758f3a5ea00191a565575"}

  def fixture(:user) do
    {:ok, user} = Accounts.create_user(@user_attrs)
    user
  end

  describe "new session" do
    test "renders form", %{conn: conn} do
      conn = get(conn, Routes.session_path(conn, :new))
      assert html_response(conn, 200) =~ "Sign in"
    end
  end

  describe "create session" do
    setup [:create_user]

    test "redirects to main page when authenticated", %{conn: conn} do
      conn = post(conn, Routes.session_path(conn, :create), @auth_success)
      assert redirected_to(conn) == Routes.page_path(conn, :index)
    end

    test "renders errors when authentication fails", %{conn: conn} do
      conn = post(conn, Routes.session_path(conn, :create), @auth_fail)
      assert redirected_to(conn) == Routes.session_path(conn, :new)
    end
  end

  describe "delete session" do
    setup [:create_user]

    test "deletes session", %{conn: conn} do
      conn = delete(conn, Routes.session_path(conn, :delete))
      assert redirected_to(conn) == Routes.page_path(conn, :index)
    end
  end

  describe "authenticate" do
    setup [:create_user]

    test "authenticate first message ok", %{conn: conn} do
      conn = post(conn, Routes.session_path(conn, :authenticate), @auth_client_first)
      assert json_response(conn, 200)
    end
  end

  defp create_user(_) do
    user = fixture(:user)
    %{user: user}
  end
end
