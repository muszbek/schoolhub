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
  @auth_client_first 'n,,n=some username,r=80151d1f366758f3a5ea00191a565575'
  @auth_client_first_wrong_user 'n,,n=wrong username,r=80151d1f366758f3a5ea00191a565575'
  @password 'some password'
  @wrong_password 'wrong password'
  

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

    test "authenticate ok", %{conn: conn} do
      conn = post(conn, Routes.session_path(conn, :authenticate), data: @auth_client_first)
      assert %{"data" => serverFirst} = json_response(conn, 200)

      auth_client_final = auth_client_final(serverFirst, @password)
      conn = post(conn, Routes.session_path(conn, :authenticate), data: auth_client_final)
      assert %{"data" => _serverFinal} = json_response(conn, 200)
    end

    test "auth wrong password ok", %{conn: conn} do
      conn = post(conn, Routes.session_path(conn, :authenticate), data: @auth_client_first)
      assert %{"data" => serverFirst} = json_response(conn, 200)

      auth_client_final = auth_client_final(serverFirst, @wrong_password)
      conn = post(conn, Routes.session_path(conn, :authenticate), data: auth_client_final)
      assert %{"data" => "e=stored_key_mismatch"} = json_response(conn, 200)
    end

    test "auth wrong user ok", %{conn: conn} do
      conn = post(conn, Routes.session_path(conn, :authenticate),
	data: @auth_client_first_wrong_user)
      assert %{"data" => "e=unknown_user"} = json_response(conn, 200)
    end
  end

  defp create_user(_) do
    user = fixture(:user)
    %{user: user}
  end

  
  defp auth_client_final(serverFirst, password) do
    %{message: 'server-first-message',
      salt: salt,
      "iteration-count": ic,
      nonce: nonce,
      str: server_first} = :scramerl_lib.parse(serverFirst)
    client_first_bare = :scramerl_lib.prune(:"gs2-header", @auth_client_first)

    salt = :base64.decode(salt)
    normalized_pw = :stringprep.prepare(password)
    salted_pw = :scramerl_lib.hi(normalized_pw, salt, ic)
    auth_msg = client_first_bare ++ ',' ++ server_first ++ ',c=biws,r=' ++ nonce
    
    _msg = :scramerl.client_final_message(nonce, salted_pw, auth_msg)
  end
end
