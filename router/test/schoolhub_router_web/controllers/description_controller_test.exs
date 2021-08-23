defmodule SchoolhubRouterWeb.DescriptionControllerTest do
  use SchoolhubRouterWeb.ConnCase

  test "GET /desctiption", %{conn: conn} do
    conn = get(conn, Routes.description_path(conn, :index))
    assert html_response(conn, 200) =~ "What is Schoolhub?"
  end

  test "GET /contact", %{conn: conn} do
    conn = get(conn, Routes.description_path(conn, :contact))
    assert html_response(conn, 200) =~ "Contact"
  end

end
