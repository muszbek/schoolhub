defmodule SchoolhubRouterWeb.PageController do
  use SchoolhubRouterWeb, :controller
  
  def index(conn, _params) do
    server_name = fetch_cookies(conn)
    |> Map.from_struct()
    |> get_in([:cookies, "server-name"])
    
    render(conn, "index.html")
  end

  def phoenix(conn, _params) do
    render(conn, "phoenix.html")
  end
end
