defmodule SchoolhubRouterWeb.DescriptionController do
  use SchoolhubRouterWeb, :controller

  def index(conn, _params) do
    render(conn, "index.html")
  end

  def contact(conn, _params) do
    render(conn, "contact.html")
  end

  def terms(conn, _params) do
    render(conn, "terms.html")
  end

  def privacy(conn, _params) do
    render(conn, "privacy.html")
  end
end
