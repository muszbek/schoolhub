defmodule SchoolhubWeb.PageController do
  use SchoolhubWeb, :controller

  def index(conn, _params) do
    render(conn, "index.html")
  end
end
