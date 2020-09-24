defmodule SchoolhubWeb.Plugs do
  use SchoolhubWeb, :router
  
  alias Schoolhub.{Accounts, Courses}
  
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


  def need_owner_aff(conn, _) do
    course_id = get_course_id(conn)
    user = %{id: user_id} = get_user(conn)
    affiliation = Courses.get_affiliation_by_user!(course_id, user_id)

    conn
    |> check_if_admin(user)
    |> check_if_owner(affiliation)
  end

  defp check_if_owner({:authorized, conn}, _affiliation), do: conn
  defp check_if_owner({:check, conn}, affiliation) do
    case affiliation.affiliation do
      "owner" -> conn
      _other ->
	conn
	|> Phoenix.Controller.put_flash(:error, "Owner affiliation required")
	|> Phoenix.Controller.redirect(to: "/courses/" <> get_course_id(conn))
	|> halt()
    end
  end

  def need_assistant_aff(conn, _) do
    course_id = get_course_id(conn)
    user = %{id: user_id} = get_user(conn)
    affiliation = Courses.get_affiliation_by_user!(course_id, user_id)

    conn
    |> check_if_admin(user)
    |> check_if_assistant(affiliation)
  end

  defp check_if_assistant({:authorized, conn}, _affiliation), do: conn
  defp check_if_assistant({:check, conn}, affiliation) do
    case affiliation.affiliation do
      "owner" -> conn
      "assistant" -> conn
      _other ->
	conn
	|> Phoenix.Controller.put_flash(:error, "Owner affiliation required")
	|> Phoenix.Controller.redirect(to: "/courses/" <> get_course_id(conn))
	|> halt()
    end
  end

  defp check_if_admin(conn, user) do
    case user.privilege.level do
      "admin" -> {:authorized, conn}
      _other -> {:check, conn}
    end
  end

  defp get_course_id(conn) do
    case conn.path_params do
      %{"course_id" => course_id} -> course_id
      %{"id" => course_id} -> course_id
    end
  end

end
