defmodule SchoolhubWeb.PageControllerTest do
  use SchoolhubWeb.ConnCase

  test "GET /phoenix", %{conn: conn} do
    conn = get(conn, "/phoenix")
    assert html_response(conn, 200) =~ "Welcome to Phoenix!"
  end

  test "GET /", %{conn: conn} do
    conn = get(conn, "/")
    assert html_response(conn, 200) =~ "Welcome to Schoolhub!"
  end
end
