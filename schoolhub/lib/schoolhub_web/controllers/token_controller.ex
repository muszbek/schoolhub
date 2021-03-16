defmodule SchoolhubWeb.TokenController do
  use SchoolhubWeb, :controller

  alias Schoolhub.Courses
  require Logger

  def new(conn, _) do
    render(conn, "new.html")
  end

  def create(conn, form_data = %{"token" => token}) do
    user_id = get_session(conn, :user_id)
    join_result = Courses.join_course_with_token(user_id, token)

    case join_result do
      {:ok, affiliation} ->
	course_id = affiliation.course_id

	conn
	|> put_flash(:info, "Succesfully joined course")
	|> redirect(to: Routes.course_path(conn, :show, course_id))
	
      {:error, %Ecto.Changeset{changes: %{course_id: course_id},
			       errors: [{:course_id,
					 {"has already been taken",
					  [constraint: :unique,
					   constraint_name: "course_affiliations_course_id_user_id_index"]}}]}} ->
	
	conn
	|> put_flash(:info, "Already member")
	|> redirect(to: Routes.course_path(conn, :show, course_id))
	
      {:error, reason} ->
	conn
	|> put_flash(:error, "Token error: " <> inspect(reason))
	|> redirect(to: Routes.token_path(conn, :new))
    end
    
  end
  
end
