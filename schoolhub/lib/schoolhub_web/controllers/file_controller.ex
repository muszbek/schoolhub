defmodule SchoolhubWeb.FileController do
  use SchoolhubWeb, :controller

  alias Schoolhub.Files
  alias Schoolhub.Files.File

  def index(conn, _params) do
    files = Files.list_files()
    render(conn, "index.html", files: files)
  end

  def new(conn, _params) do
    changeset = Files.change_file(%File{})
    render(conn, "new.html", changeset: changeset)
  end

  def create(conn, %{"file" => file_params}) do
    case Files.create_file(file_params) do
      {:ok, file} ->
        conn
        |> put_flash(:info, "File created successfully.")
        |> redirect(to: Routes.file_path(conn, :show, file))

      {:error, %Ecto.Changeset{} = changeset} ->
        render(conn, "new.html", changeset: changeset)
    end
  end

  def show(conn, %{"id" => id}) do
    file = Files.get_file!(id)
    render(conn, "show.html", file: file)
  end

  def edit(conn, %{"id" => id}) do
    file = Files.get_file!(id)
    changeset = Files.change_file(file)
    render(conn, "edit.html", file: file, changeset: changeset)
  end

  def update(conn, %{"id" => id, "file" => file_params}) do
    file = Files.get_file!(id)

    case Files.update_file(file, file_params) do
      {:ok, file} ->
        conn
        |> put_flash(:info, "File updated successfully.")
        |> redirect(to: Routes.file_path(conn, :show, file))

      {:error, %Ecto.Changeset{} = changeset} ->
        render(conn, "edit.html", file: file, changeset: changeset)
    end
  end

  def delete(conn, %{"id" => id}) do
    file = Files.get_file!(id)
    {:ok, _file} = Files.delete_file(file)

    conn
    |> put_flash(:info, "File deleted successfully.")
    |> redirect(to: Routes.file_path(conn, :index))
  end
end
