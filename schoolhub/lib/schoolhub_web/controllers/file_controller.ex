defmodule SchoolhubWeb.FileController do
  use SchoolhubWeb, :controller
  
  alias Schoolhub.Files
  alias Schoolhub.Files.File

  def index(conn, %{"course_id" => course_id}) do
    files = Files.list_files()
    render(conn, "index.html", files: files, course_id: course_id)
  end

  def new(conn, %{"course_id" => course_id}) do
    changeset = Files.change_file(%File{})
    render(conn, "new.html", changeset: changeset, course_id: course_id)
  end

  def create(conn, %{"course_id" => course_id, "file" => file_params}) do
    user_id = get_session(conn, :user_id)
    file_params_with_uploader = file_params
    |> Map.put("uploader", user_id)
    |> Morphix.atomorphify!()
    
    case Files.create_file(file_params_with_uploader) do
      {:ok, file} ->
        conn
        |> put_flash(:info, "File created successfully.")
        |> redirect(to: Routes.course_file_path(conn, :show, course_id, file))

      {:error, %Ecto.Changeset{} = changeset} ->
        render(conn, "new.html", changeset: changeset, course_id: course_id)
    end
  end

  def show(conn, %{"course_id" => course_id, "id" => id}) do
    file = Files.get_file!(id)
    render(conn, "show.html", file: file, course_id: course_id)
  end

  def edit(conn, %{"course_id" => course_id, "id" => id}) do
    file = Files.get_file!(id)
    changeset = Files.change_file(file)
    render(conn, "edit.html", file: file, changeset: changeset, course_id: course_id)
  end

  def update(conn, %{"course_id" => course_id, "id" => id, "file" => file_params}) do
    file = Files.get_file!(id)

    case Files.update_file(file, file_params) do
      {:ok, file} ->
        conn
        |> put_flash(:info, "File updated successfully.")
        |> redirect(to: Routes.course_file_path(conn, :show, course_id, file))

      {:error, %Ecto.Changeset{} = changeset} ->
        render(conn, "edit.html", file: file, changeset: changeset, course_id: course_id)
    end
  end

  def delete(conn, %{"course_id" => course_id, "id" => id}) do
    file = Files.get_file!(id)
    {:ok, _file} = Files.delete_file(file)

    conn
    |> put_flash(:info, "File deleted successfully.")
    |> redirect(to: Routes.course_file_path(conn, :index, course_id))
  end
end
