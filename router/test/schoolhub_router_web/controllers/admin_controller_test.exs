defmodule SchoolhubRouterWeb.AdminControllerTest do
  use SchoolhubRouterWeb.ConnCase

  @admin_password "test_password"
  @incorrect_password "incorrect_password"
  
  describe "index" do
    test "loads admin authorization form", %{conn: conn} do
      conn = get(conn, Routes.admin_path(conn, :index))
      assert html_response(conn, 200) =~ "Admin authorization"
    end
  end

  describe "verify admin password" do
    test "redirects to admin panel if password is correct", %{conn: conn} do
      conn = post(conn, Routes.admin_path(conn, :verify), admin_auth: @admin_password)
      assert redirected_to(conn) == Routes.admin_path(conn, :panel)
    end

    test "redirects to root if password is incorrect", %{conn: conn} do
      conn = post(conn, Routes.admin_path(conn, :verify), admin_auth: @incorrect_password)
      assert redirected_to(conn) == Routes.page_path(conn, :index)
    end
  end

  describe "panel" do
    test "redirects to root because no header", %{conn: conn} do
      conn = get(conn, Routes.admin_path(conn, :panel))
      assert redirected_to(conn) == Routes.page_path(conn, :index)
    end
  end
  
end
