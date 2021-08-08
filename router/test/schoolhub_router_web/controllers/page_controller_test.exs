defmodule SchoolhubRouterWeb.PageControllerTest do
  use SchoolhubRouterWeb.ConnCase

  test "GET /phoenix", %{conn: conn} do
    conn = get(conn, Routes.page_path(conn, :phoenix))
    assert html_response(conn, 200) =~ "Welcome to Phoenix!"
  end

  test "GET /", %{conn: conn} do
    conn = get(conn, Routes.page_path(conn, :index))
    assert html_response(conn, 200) =~ "Welcome to Schoolhub!"
  end
end
