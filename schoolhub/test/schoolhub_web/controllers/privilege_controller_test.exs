defmodule SchoolhubWeb.PrivilegeControllerTest do
  use SchoolhubWeb.ConnCase

  alias Schoolhub.Privileges

  @create_attrs %{level: "some level"}
  @update_attrs %{level: "some updated level"}
  @invalid_attrs %{level: nil}

  def fixture(:privilege) do
    {:ok, privilege} = Privileges.create_privilege(@create_attrs)
    privilege
  end

  describe "index" do
    test "lists all privileges", %{conn: conn} do
      conn = get(conn, Routes.privilege_path(conn, :index))
      assert html_response(conn, 200) =~ "Listing Privileges"
    end
  end

  describe "new privilege" do
    test "renders form", %{conn: conn} do
      conn = get(conn, Routes.privilege_path(conn, :new))
      assert html_response(conn, 200) =~ "New Privilege"
    end
  end

  describe "create privilege" do
    test "redirects to show when data is valid", %{conn: conn} do
      conn = post(conn, Routes.privilege_path(conn, :create), privilege: @create_attrs)

      assert %{id: id} = redirected_params(conn)
      assert redirected_to(conn) == Routes.privilege_path(conn, :show, id)

      conn = get(conn, Routes.privilege_path(conn, :show, id))
      assert html_response(conn, 200) =~ "Show Privilege"
    end

    test "renders errors when data is invalid", %{conn: conn} do
      conn = post(conn, Routes.privilege_path(conn, :create), privilege: @invalid_attrs)
      assert html_response(conn, 200) =~ "New Privilege"
    end
  end

  describe "edit privilege" do
    setup [:create_privilege]

    test "renders form for editing chosen privilege", %{conn: conn, privilege: privilege} do
      conn = get(conn, Routes.privilege_path(conn, :edit, privilege))
      assert html_response(conn, 200) =~ "Edit Privilege"
    end
  end

  describe "update privilege" do
    setup [:create_privilege]

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

  describe "delete privilege" do
    setup [:create_privilege]

    test "deletes chosen privilege", %{conn: conn, privilege: privilege} do
      conn = delete(conn, Routes.privilege_path(conn, :delete, privilege))
      assert redirected_to(conn) == Routes.privilege_path(conn, :index)
      assert_error_sent 404, fn ->
        get(conn, Routes.privilege_path(conn, :show, privilege))
      end
    end
  end

  defp create_privilege(_) do
    privilege = fixture(:privilege)
    %{privilege: privilege}
  end
end
