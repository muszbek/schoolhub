defmodule SchoolhubRouterWeb.AdminController do
  use SchoolhubRouterWeb, :controller

  alias SchoolhubRouter.AdminLib
  alias SchoolhubRouter.Instances
  alias SchoolhubRouter.Instances.Server
  alias SchoolhubRouterWeb.ServerController
  
  def index(conn, _params) do
    render(conn, "index.html")
  end

  def verify(conn, %{"admin_auth" => password}) do
    case AdminLib.verify_password({:ok, password}) do
      :ok ->
	admin_token = AdminLib.create_token(password)
	
	conn
	|> put_resp_cookie("admin_token", admin_token)
	|> redirect(to: Routes.admin_path(conn, :panel))
      {:error, _error} ->
	conn
	|> Phoenix.Controller.put_flash(:error, "Admin authorization failed")
	|> Phoenix.Controller.redirect(to: Routes.page_path(conn, :index))
    end
  end

  def panel(conn, _params) do
    changeset = Instances.change_server(%Server{})
    render(conn, "panel.html", changeset: changeset)
  end

  def create(conn, %{"server" => server_params}) do
    ServerController.do_create(conn, %{"server" => server_params})
  end
  
  def admin_unsubscribe(conn, %{"name" => server_name}) do
    ServerController.do_unsubscribe(conn, server_name)
  end
end
