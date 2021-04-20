defmodule SchoolhubWeb.LayoutView do
  use SchoolhubWeb, :view

  alias Plug.Conn
  alias Schoolhub.Accounts

  
  def get_session_url(conn) do
    case Conn.get_session(conn, :user_id) do
      nil -> Routes.session_path(conn, :new)
      _id -> Routes.user_path(conn, :show_self)
    end
  end

  def get_session_link_text(conn) do
    case Conn.get_session(conn, :user_id) do
      nil ->
	"Session"
      id ->
	user = Accounts.get_user!(id)
	user.credential.username
    end
  end
  
end
