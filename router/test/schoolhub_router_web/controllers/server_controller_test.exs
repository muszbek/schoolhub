defmodule SchoolhubRouterWeb.ServerControllerTest do
  use SchoolhubRouterWeb.ConnCase

  alias SchoolhubRouter.Instances

  @create_attrs %{name: "some_name", admin_pw: "some_pw"}
  @create_raw_attrs %{name: "some_name", address: "some_address", admin_pw: "some_pw"}
  @invalid_attrs %{name: nil, admin_pw: nil}

  def fixture(:server) do
    {:ok, server} = Instances.create_server(@create_raw_attrs)
    server
  end

  
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
    #test "redirects to root when data is valid", %{conn: conn} do
    #  conn = post(conn, Routes.server_path(conn, :create), server: @create_attrs)
    #  assert redirected_to(conn) == Routes.page_path(conn, :index)
    #end

    test "renders errors when data is invalid", %{conn: conn} do
      conn = post(conn, Routes.server_path(conn, :create), server: @invalid_attrs)
      assert html_response(conn, 200) =~ "New Server"
    end

    test "redirects to new server when cannot connect to k8s", %{conn: conn} do
      conn = post(conn, Routes.server_path(conn, :create), server: @create_attrs)
      assert redirected_to(conn) == Routes.server_path(conn, :new)
    end
  end

  describe "redirect to instance" do
    setup [:create_server]
    
    test "redirects to main page when server does not exist", %{conn: conn} do
      conn = post(conn, Routes.server_path(conn, :to_instance), server_name: "some_invalid_name")
      assert redirected_to(conn) == Routes.page_path(conn, :index)
    end

    test "redirects to server address when server exists", %{conn: conn, server: server} do
      conn = post(conn, Routes.server_path(conn, :to_instance), server_name: server.name)
      assert redirected_to(conn) == "/" <> server.address <> "/"
    end
  end


  defp create_server(_) do
    server = fixture(:server)
    %{server: server}
  end
end
