defmodule SchoolhubWeb.PageController do
  use SchoolhubWeb, :controller

  def index(conn, _params) do
    render(conn, "index.html")
  end

  def phoenix(conn, _params) do
    render(conn, "phoenix.html")
  end
end
