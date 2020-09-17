defmodule SchoolhubWeb.PrivilegeController do
  use SchoolhubWeb, :controller

  alias Schoolhub.Privileges
  alias Schoolhub.Privileges.Privilege

  def index(conn, _params) do
    privileges = Privileges.list_privileges()
    render(conn, "index.html", privileges: privileges)
  end

  def new(conn, _params) do
    changeset = Privileges.change_privilege(%Privilege{})
    render(conn, "new.html", changeset: changeset)
  end

  def create(conn, %{"privilege" => privilege_params}) do
    case Privileges.create_privilege(privilege_params) do
      {:ok, privilege} ->
        conn
        |> put_flash(:info, "Privilege created successfully.")
        |> redirect(to: Routes.privilege_path(conn, :show, privilege))

      {:error, %Ecto.Changeset{} = changeset} ->
        render(conn, "new.html", changeset: changeset)
    end
  end

  def show(conn, %{"id" => id}) do
    privilege = Privileges.get_privilege!(id)
    render(conn, "show.html", privilege: privilege)
  end

  def edit(conn, %{"id" => id}) do
    privilege = Privileges.get_privilege!(id)
    changeset = Privileges.change_privilege(privilege)
    render(conn, "edit.html", privilege: privilege, changeset: changeset)
  end

  def update(conn, %{"id" => id, "privilege" => privilege_params}) do
    privilege = Privileges.get_privilege!(id)

    case Privileges.update_privilege(privilege, privilege_params) do
      {:ok, privilege} ->
        conn
        |> put_flash(:info, "Privilege updated successfully.")
        |> redirect(to: Routes.privilege_path(conn, :show, privilege))

      {:error, %Ecto.Changeset{} = changeset} ->
        render(conn, "edit.html", privilege: privilege, changeset: changeset)
    end
  end

  def delete(conn, %{"id" => id}) do
    privilege = Privileges.get_privilege!(id)
    {:ok, _privilege} = Privileges.delete_privilege(privilege)

    conn
    |> put_flash(:info, "Privilege deleted successfully.")
    |> redirect(to: Routes.privilege_path(conn, :index))
  end
end
