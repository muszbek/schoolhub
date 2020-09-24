defmodule SchoolhubWeb.Plugs do
  use SchoolhubWeb, :router

  alias Schoolhub.Accounts
  
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

  defp redirect_to_login(conn) do
    conn
    |> Phoenix.Controller.put_flash(:error, "Login required")
    |> Phoenix.Controller.redirect(to: "/sessions/new")
    |> halt()
  end

  
  def need_admin_priv(conn, _) do
    user = get_user(conn)

    case user.privilege.level do
      "admin" -> conn
      _other ->
	conn
	|> Phoenix.Controller.put_flash(:error, "Admin privilige required")
	|> Phoenix.Controller.redirect(to: "/")
	|> halt()
    end
  end
    
  def need_teacher_priv(conn, _) do
    user = get_user(conn)

    case user.privilege.level do
      "admin" -> conn
      "teacher" -> conn
      _other ->
	conn
	|> Phoenix.Controller.put_flash(:error, "Teacher privilige required")
	|> Phoenix.Controller.redirect(to: "/courses")
	|> halt()
    end
  end

  defp get_user(conn) do
    conn
    |> get_session(:user_id)
    |> Accounts.get_user!()
  end

end
