defmodule SchoolhubRouterWeb.DescriptionController do
  use SchoolhubRouterWeb, :controller

  def index(conn, _params) do
    render(conn, "index.html")
  end
end
