defmodule SchoolhubRouterWeb.PageController do
  use SchoolhubRouterWeb, :controller

  alias SchoolhubRouterWeb.ServerController, as: Server
  
  def index(conn, _params) do
    case server_name_from_cookie(conn) do
      nil -> render_root(conn)
      "" -> render_root(conn)
      name -> Server.to_instance(conn, %{"server_name" => name})
    end  
  end

  defp render_root(conn) do
    conn
    |> put_resp_cookie("admin_auth", "")
    |> render("index.html")
  end

  def phoenix(conn, _params) do
    render(conn, "phoenix.html")
  end

  defp server_name_from_cookie(conn) do
    conn
    |> fetch_cookies()
    |> Map.from_struct()
    |> get_in([:cookies, "server-name"])
  end
end
