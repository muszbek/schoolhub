defmodule SchoolhubWeb.PageControllerTest do
  use SchoolhubWeb.ConnCase

  test "GET /phoenix", %{conn: conn} do
    conn = get(conn, Routing.route(:page_path, conn, [:phoenix]))
    assert html_response(conn, 200) =~ "Welcome to Phoenix!"
  end

  test "GET /", %{conn: conn} do
    conn = get(conn, Routing.route(:page_path, conn, [:index]))
    assert html_response(conn, 200) =~ "Welcome to Schoolhub!"
  end
end
