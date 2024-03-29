defmodule SchoolhubRouterWeb.ServerControllerTest do
  use SchoolhubRouterWeb.ConnCase

  alias SchoolhubRouter.Instances

  @pod_address_suffix ".schoolhub.default.svc.cluster.local"
#  @create_attrs %{name: "some_name", owner_email: "some_email", admin_pw: "some_pw",
#		  price_id: "price_1JfOXQI0DC66QfKHyh8H0fpj"}
  @create_raw_attrs %{name: "some_name", address: "some_address" <> @pod_address_suffix,
		      admin_pw: "some_pw", owner_email: "some_email"}
  @invalid_attrs %{name: nil, admin_pw: nil, owner_email: nil}

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

  describe "success" do
    test "shows success page", %{conn: conn} do
      conn = get(conn, Routes.server_path(conn, :success))
      assert html_response(conn, 200) =~ "Subscribed"
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

  describe "get admin password" do
    setup [:create_server]

    test "gets admin password and then gets nothing", %{conn: conn, server: server} do
      conn = get(conn, Routes.server_path(conn, :get_admin_pw, "some_address"))
      assert json_response(conn, 200)["already_injected"] == false
      assert json_response(conn, 200)["admin_pw"] == server.admin_pw

      conn = get(conn, Routes.server_path(conn, :get_admin_pw, "some_address"))
      assert json_response(conn, 200)["already_injected"] == true
      assert json_response(conn, 200)["admin_pw"] == nil
    end
  end

  describe "unsubscribe page" do
    test "renders form", %{conn: conn} do
      conn = get(conn, Routes.server_path(conn, :unsubscribe))
      assert html_response(conn, 200) =~ "Unsubscribe"
    end
  end

  describe "unsubscribe server form" do
    setup [:create_server]
    
    test "valid credentials pass", %{conn: conn, server: server} do
      conn = post(conn, Routes.server_path(conn, :email_unsubscribe),
	name: server.name, email: server.owner_email)
      assert redirected_to(conn) == Routes.page_path(conn, :index)
    end

    test "invalid name does not pass", %{conn: conn} do
      conn = post(conn, Routes.server_path(conn, :email_unsubscribe),
	name: "some_invalid_name", email: nil)
      assert redirected_to(conn) == Routes.server_path(conn, :unsubscribe)
    end

    test "not matching email does not pass", %{conn: conn, server: server} do
      conn = post(conn, Routes.server_path(conn, :email_unsubscribe),
	name: server.name, email: "some_invalid_email")
      assert redirected_to(conn) == Routes.server_path(conn, :unsubscribe)
    end
  end


  defp create_server(_) do
    server = fixture(:server)
    %{server: server}
  end
end
