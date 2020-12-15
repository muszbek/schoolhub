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

    conn
    |> check_if_admin(user)
    |> check_if_owner(course_id, user_id)
  end

  defp check_if_owner({:authorized, conn}, _course_id, _user_id), do: conn
  defp check_if_owner({:check, conn}, course_id, user_id) do
    %{affiliation: affiliation} = Courses.get_affiliation_by_user!(course_id, user_id)
    
    case affiliation do
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

    conn
    |> check_if_admin(user)
    |> check_if_assistant(course_id, user_id)
  end

  defp check_if_assistant({:authorized, conn}, _course_id, _user_id), do: conn
  defp check_if_assistant({:check, conn}, course_id, user_id) do
    %{affiliation: affiliation} = Courses.get_affiliation_by_user!(course_id, user_id)
    
    case affiliation do
      "owner" -> conn
      "assistant" -> conn
      _other ->
	conn
	|> Phoenix.Controller.put_flash(:error, "Assistant affiliation required")
	|> Phoenix.Controller.redirect(to: "/courses/" <> get_course_id(conn))
	|> halt()
    end
  end

  
  def need_self_aff(conn, _) do
    course_id = get_course_id(conn)
    user = %{id: user_id} = get_user(conn)

    conn
    |> check_if_admin(user)
    |> check_if_self(course_id, user_id)
  end

  defp check_if_self({:authorized, conn}, _course_id, _user_id), do: conn
  defp check_if_self({:check, conn}, course_id, user_id) do
    %{affiliation: affiliation} = Courses.get_affiliation_by_user!(course_id, user_id)
    
    case affiliation do
      "owner" -> conn
      "assistant" -> conn
      _other ->
	case affiliation.id == get_aff_id(conn) do
	  true -> conn
	  _ ->
	    conn
	    |> Phoenix.Controller.put_flash(:error, "Assistant affiliation required")
	    |> Phoenix.Controller.redirect(to: "/courses/" <> get_course_id(conn) <>
	      "/affiliations")
	    |> halt()
	end
    end
  end

  
  def need_aff(conn, _) do
    course_id = get_course_id(conn)
    user = %{id: user_id} = get_user(conn)

    conn
    |> check_if_admin(user)
    |> check_if_affiliated(course_id, user_id)
  end

  defp check_if_affiliated({:authorized, conn}, _course_id, _user_id), do: conn
  defp check_if_affiliated({:check, conn}, course_id, user_id) do
    _affiliation = Courses.get_affiliation_by_user!(course_id, user_id)
    ## fetching the affiliation already checks this,
    ## it crashes if there is no affiliation
    conn
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

  defp get_aff_id(conn) do
    case conn.path_params do
      %{"affiliation_id" => aff_id} -> aff_id
      _other -> {:error, :session_not_affiliated}
    end
  end

end
