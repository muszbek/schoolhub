defmodule SchoolhubWeb.PrivilegeControllerTest do
  use SchoolhubWeb.ConnCase

  alias Schoolhub.Accounts

  @update_attrs %{level: "teacher"}
  @invalid_attrs %{level: "some invalid level"}
  
  @create_user_attrs %{email: "some email",
		       name: "some name",
		       credential: %{username: "some username",
				     password: "some password"}}

  def fixture(:user) do
    {:ok, user} = Accounts.create_user(@create_user_attrs)
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
    
    test "lists all privileges", %{conn: conn} do
      conn = get(conn, Routes.privilege_path(conn, :index))
      assert html_response(conn, 200) =~ "Listing Privileges"
    end
  end

  describe "edit privilege" do
    setup [:create_user]
    setup [:enter_session]

    test "renders form for editing chosen privilege", %{conn: conn, privilege: privilege} do
      conn = get(conn, Routes.privilege_path(conn, :edit, privilege))
      assert html_response(conn, 200) =~ "Edit Privilege"
    end
  end

  describe "update privilege" do
    setup [:create_user]
    setup [:enter_session]

    test "redirects when data is valid", %{conn: conn, privilege: privilege} do
      conn = put(conn, Routes.privilege_path(conn, :update, privilege), privilege: @update_attrs)
      assert redirected_to(conn) == Routes.privilege_path(conn, :show, privilege)

      conn = get(conn, Routes.privilege_path(conn, :show, privilege))
      assert html_response(conn, 200) =~ "some updated level"
    end

    test "renders errors when data is invalid", %{conn: conn, privilege: privilege} do
      conn = put(conn, Routes.privilege_path(conn, :update, privilege), privilege: @invalid_attrs)
      assert html_response(conn, 200) =~ "Edit Privilege"
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
