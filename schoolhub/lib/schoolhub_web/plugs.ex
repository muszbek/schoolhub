defmodule SchoolhubWeb.Plugs do
  use SchoolhubWeb, :router
  
  def authenticate_user(conn, _) do
    case get_session(conn, :user_id) do
      nil ->
        redirect_to_login(conn)
      user_id ->
	try do
          assign(conn, :current_user, Schoolhub.Accounts.get_user!(user_id))
	rescue
	  Ecto.NoResultsError ->
	    redirect_to_login(conn)
	end
    end
  end

  def redirect_to_login(conn) do
    conn
    |> Phoenix.Controller.put_flash(:error, "Login required")
    |> Phoenix.Controller.redirect(to: "/sessions/new")
    |> halt()
  end
end
