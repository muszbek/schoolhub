defmodule SchoolhubRouterWeb.AdminController do
  use SchoolhubRouterWeb, :controller

  alias SchoolhubRouter.AdminLib
  
  def index(conn, _params) do
    render(conn, "index.html")
  end

  def verify(conn, %{"admin_auth" => password}) do
    admin_token = AdminLib.create_token(password)
    
    conn
    |> put_req_header("admin_token", admin_token)
    |> redirect(to: Routes.admin_path(conn, :panel))
  end

  def panel(conn, _params) do
    render(conn, "panel.html")
  end
end
