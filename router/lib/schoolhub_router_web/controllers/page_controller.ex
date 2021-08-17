defmodule SchoolhubRouterWeb.PageController do
  use SchoolhubRouterWeb, :controller

  alias SchoolhubRouterWeb.ServerController, as: Server
  
  def index(conn, _params) do
    case server_name_from_cookie(conn) do
      nil -> render(conn, "index.html")
      "" -> render(conn, "index.html")
      name -> Server.to_instance(conn, %{"server_name" => name})
    end  
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
