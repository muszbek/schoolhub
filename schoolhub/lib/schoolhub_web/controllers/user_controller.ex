defmodule SchoolhubWeb.UserController do
  use SchoolhubWeb, :controller

  alias Schoolhub.Accounts
  alias Schoolhub.Accounts.User
  alias Schoolhub.{Mailer, Email}
  
  def index(conn, _params) do
    users = Accounts.list_users()
    render(conn, "index.html", users: users)
  end

  def new(conn, _params) do
    changeset = Accounts.change_user(%User{})
    render(conn, "new.html", changeset: changeset)
  end

  def create(conn, %{"user" => user_params}) do
    case Accounts.create_user(user_params) do
      {:ok, user} ->
	## Add again when confirmed from email
	## Adding now is necessary to detect changeset errors
	Accounts.delete_user(user)

	Email.confirm_reg_email(user_params)
	|> Mailer.deliver_now!()
	
        conn
        |> put_flash(:info, "Confiramtion email sent to register.")
        |> redirect(to: Routing.route(:session_path, conn, [:new]))

      {:error, %Ecto.Changeset{} = changeset} ->
        render(conn, "new.html", changeset: changeset)
    end
  end

  def confirm(conn, %{"token" => token}) do
    token_result = Accounts.verify_token(token)

    case token_result do
      {:ok, user_params} ->
	do_confirm(conn, user_params)
      {:error, _reason} ->
	conn
	|> put_flash(:error, "Confirm registration token error")
	|> redirect(to:	Routing.route(:session_path, conn, [:new]))
    end
  end

  defp do_confirm(conn, user_params) do
    case Accounts.create_user(user_params) do
      {:ok, _user} ->
        conn
        |> put_flash(:info, "User registration confirmed.")
        |> redirect(to: Routing.route(:session_path, conn, [:new]))

      {:error, %Ecto.Changeset{}} ->
        conn
        |> put_flash(:error, "Something went wrong with user registration.")
        |> redirect(to: Routing.route(:session_path, conn, [:new]))
    end
  end

  def show(conn, %{"id" => id}) do
    user = Accounts.get_user!(id)
    render(conn, "show.html", user: user, self: false)
  end

  def show_self(conn, _) do
    id = get_session(conn, :user_id)
    user = Accounts.get_user!(id)
    render(conn, "show.html", user: user, self: true)
  end

  def edit(conn, %{"id" => id}) do
    user = Accounts.get_user!(id)
    changeset = Accounts.change_user(user)
    render(conn, "edit.html", user: user, changeset: changeset)
  end

  def update(conn, %{"id" => id, "user" => user_params}) do
    user = Accounts.get_user!(id)

    case Accounts.update_user(user, user_params) do
      {:ok, user} ->
        conn
        |> put_flash(:info, "User updated successfully.")
        |> redirect(to: Routing.route(:user_path, conn, [:show, user]))

      {:error, %Ecto.Changeset{} = changeset} ->
        render(conn, "edit.html", user: user, changeset: changeset)
    end
  end

  def delete(conn, %{"id" => id}) do
    user_id = get_session(conn, :user_id)
    id = String.to_integer(id)
    
    case user_id == id do
      false ->
	user = Accounts.get_user!(id)

	conn
	|> try_to_delete(user)
	|> redirect(to: Routing.route(:user_path, conn, [:index]))
      true ->
	conn
	|> put_flash(:error, "Cannot delete self this way.")
	|> redirect(to: Routing.route(:user_path, conn, [:index]))
    end
  end

  def self_delete(conn, _params) do
    user_id = get_session(conn, :user_id)
    user = Accounts.get_user!(user_id)

    conn
    |> try_to_delete(user)
    |> configure_session(drop: true)
    |> redirect(to: Routing.route(:session_path, conn, [:new]))
  end

  defp try_to_delete(conn, user) do
    ## puts flash message but no redirection
    try do
      {:ok, _user} = Accounts.delete_user(user)
      
      conn
      |> put_flash(:info, "User deleted successfully.")
    rescue
      Ecto.ConstraintError -> conn
      |> put_flash(:error, "User cannot be deleted because of dependencies!")
    end
  end


  def change_pw(conn, %{"token" => token}) do
    token_result = Accounts.verify_token(token)

    case token_result do
      {:ok, username} ->
	render(conn, "change_pw.html", token: token, username: username)
      {:error, :invalid} ->
	conn
	|> put_flash(:error, "Change password token invalid!")
	|> redirect(to:	Routing.route(:session_path, conn, [:new]))
      {:error, :expired} ->
	conn
	|> put_flash(:error, "Change password token expired!")
	|> redirect(to:	Routing.route(:session_path, conn, [:new]))
    end	
  end

  def update_pw(conn, %{"token" => token, "password" => password}) do
    token_result = Accounts.verify_token(token)

    case token_result do
      {:ok, username} ->
	do_update_pw(conn, username, password, token)
      {:error, _reason} ->
	conn
	|> put_flash(:error, "Update password token error")
	|> redirect(to:	Routing.route(:session_path, conn, [:new]))
    end
  end

  defp do_update_pw(conn, username, password, token) do
    cred = Accounts.get_credential!(username)

    case Accounts.update_credential(cred, %{password: password}) do
      {:ok, _cred} ->
	conn
	|> put_flash(:info, "Password successfully updated")
	|> redirect(to: Routing.route(:session_path, conn, [:new]))
      {:error, %Ecto.Changeset{}} ->
	conn
	|> put_flash(:error, "New password is invalid!")
	|> redirect(to: Routing.route(:user_path, conn, [:change_pw, token]))
    end
  end
end
