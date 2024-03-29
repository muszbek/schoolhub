defmodule SchoolhubWeb.SessionControllerTest do
  use SchoolhubWeb.ConnCase

  alias Schoolhub.Accounts

  @user_attrs %{email: "some email",
		name: "some name",
		credential: %{username: "some username",
			      password: "some password"}}
  @auth_success %{username: "some username",
		  result: "authenticated",
		  access_token: "some access token",
		  refresh_token: "some refresh token"}
  @auth_fail %{username: "some username",
	       result: "some error"}
  @auth_client_first %{data: "n,,n=some username,r=80151d1f366758f3a5ea00191a565575"}
  @new_token %{refresh_token: "some token"}

  def fixture(:user) do
    {:ok, user} = Accounts.create_user(@user_attrs)
    user
  end

  describe "new session" do
    test "renders form", %{conn: conn} do
      conn = get(conn, Routing.route(:session_path, conn, [:new]))
      assert html_response(conn, 200) =~ "Sign in"
    end
  end

  describe "create session" do
    setup [:create_user]

    test "redirects to main page when authenticated", %{conn: conn} do
      conn = post(conn, Routing.route(:session_path, conn, [:create]), @auth_success)
      assert redirected_to(conn) == Routing.route(:page_path, conn, [:index])
    end

    test "renders errors when authentication fails", %{conn: conn} do
      conn = post(conn, Routing.route(:session_path, conn, [:create]), @auth_fail)
      assert redirected_to(conn) == Routing.route(:session_path, conn, [:new])
    end
  end

  describe "delete session" do
    setup [:create_user]

    test "deletes session", %{conn: conn} do
      conn = delete(conn, Routing.route(:session_path, conn, [:delete]))
      assert redirected_to(conn) == Routing.route(:session_path, conn, [:new])
    end
  end

  describe "forgot password" do
    test "renders form", %{conn: conn} do
      conn = get(conn, Routing.route(:session_path, conn, [:forgot_pw]))
      assert html_response(conn, 200) =~ "Forgot password"
    end
  end

  describe "send email to address" do
    setup [:create_user]
    
    test "redirects to login page when email sent", %{conn: conn, user: user} do
      email = %{email: user.email}
      conn = post(conn, Routing.route(:session_path, conn, [:send_email]), email)
      assert redirected_to(conn) == Routing.route(:session_path, conn, [:new])
    end

    test "redirects to login page when email is wrong", %{conn: conn} do
      email = %{email: "invalid email address"}
      conn = post(conn, Routing.route(:session_path, conn, [:send_email]), email)
      assert redirected_to(conn) == Routing.route(:session_path, conn, [:new])
    end
  end

  describe "authenticate" do
    setup [:create_user]

    test "authenticate first message ok", %{conn: conn} do
      conn = post(conn, Routing.route(:session_path, conn, [:authenticate]), @auth_client_first)
      assert json_response(conn, 200)
    end
  end

  describe "renew_token" do
    setup [:create_user]

    test "renew token ok", %{conn: conn, user: user} do
      conn = conn
      |> Plug.Test.init_test_session(user_id: nil)
      |> SchoolhubWeb.SessionController.enter_session(user)
      
      conn = post(conn, Routing.route(:session_path, conn, [:renew_token]), @new_token)
      assert response(conn, 200)
    end
  end
  

  defp create_user(_) do
    user = fixture(:user)
    %{user: user}
  end
end
