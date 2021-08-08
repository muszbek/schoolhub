defmodule SchoolhubRouterWeb.PageController do
  use SchoolhubRouterWeb, :controller

  def index(conn, _params) do
    render(conn, "index.html")
  end

  def phoenix(conn, _params) do
    render(conn, "phoenix.html")
  end
end
