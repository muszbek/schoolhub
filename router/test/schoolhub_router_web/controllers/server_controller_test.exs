defmodule SchoolhubRouterWeb.ServerControllerTest do
  use SchoolhubRouterWeb.ConnCase

  @create_attrs %{address: "some address", name: "some name"}
  @invalid_attrs %{active: nil, address: nil, name: nil}

  
  describe "index" do
    test "lists all servers", %{conn: conn} do
      conn = get(conn, Routes.server_path(conn, :index))
      assert html_response(conn, 200) =~ "Listing Servers"
    end
  end

  describe "new server" do
    test "renders form", %{conn: conn} do
      conn = get(conn, Routes.server_path(conn, :new))
      assert html_response(conn, 200) =~ "New Server"
    end
  end

  describe "create server" do
    test "redirects to show when data is valid", %{conn: conn} do
      conn = post(conn, Routes.server_path(conn, :create), server: @create_attrs)

      assert %{id: id} = redirected_params(conn)
      assert redirected_to(conn) == Routes.server_path(conn, :show, id)

      conn = get(conn, Routes.server_path(conn, :show, id))
      assert html_response(conn, 200) =~ "Show Server"
    end

    test "renders errors when data is invalid", %{conn: conn} do
      conn = post(conn, Routes.server_path(conn, :create), server: @invalid_attrs)
      assert html_response(conn, 200) =~ "New Server"
    end
  end

end
