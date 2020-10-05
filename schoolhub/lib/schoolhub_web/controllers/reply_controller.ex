defmodule SchoolhubWeb.ReplyController do
  use SchoolhubWeb, :controller

  alias Schoolhub.Posts
  alias Schoolhub.Posts.Reply

  def index(conn, %{"course_id" => course_id, "post_id" => post_id}) do
    redirect(conn, to: Routes.course_post_path(conn, :show, course_id, post_id))
  end

  def new(conn, %{"course_id" => course_id, "post_id" => post_id}) do
    changeset = Posts.change_reply(%Reply{})
    render(conn, "new.html", changeset: changeset, course_id: course_id, post_id: post_id)
  end

  def create(conn, %{"course_id" => course_id, "post_id" => post_id, "reply" => reply_params}) do
    user_id = get_session(conn, :user_id)
    reply_params_with_creator = reply_params
    |> Map.put("creator", user_id)
    |> Map.put("parent_post", post_id)
    |> Morphix.atomorphify!()
    
    case Posts.create_reply(reply_params_with_creator) do
      {:ok, reply} ->
        conn
        |> put_flash(:info, "Reply created successfully.")
        |> redirect(to: Routes.course_post_reply_path(conn, :show, course_id, post_id, reply))

      {:error, %Ecto.Changeset{} = changeset} ->
        render(conn, "new.html", changeset: changeset, course_id: course_id, post_id: post_id)
    end
  end

  def show(conn, %{"course_id" => course_id, "post_id" => post_id, "id" => id}) do
    reply = Posts.get_reply!(id)
    render(conn, "show.html", reply: reply, course_id: course_id, post_id: post_id)
  end

  def edit(conn, %{"course_id" => course_id, "post_id" => post_id, "id" => id}) do
    reply = Posts.get_reply!(id)
    changeset = Posts.change_reply(reply)
    render(conn, "edit.html", reply: reply, changeset: changeset,
      course_id: course_id, post_id: post_id)
  end

  def update(conn, %{"course_id" => course_id, "post_id" => post_id, "id" => id,
		     "reply" => reply_params}) do
    reply = Posts.get_reply!(id)

    case Posts.update_reply(reply, reply_params) do
      {:ok, reply} ->
        conn
        |> put_flash(:info, "Reply updated successfully.")
        |> redirect(to: Routes.course_post_reply_path(conn, :show, course_id, post_id, reply))

      {:error, %Ecto.Changeset{} = changeset} ->
        render(conn, "edit.html", reply: reply, changeset: changeset,
	  course_id: course_id, post_id: post_id)
    end
  end

  def delete(conn, %{"course_id" => course_id, "post_id" => post_id, "id" => id}) do
    reply = Posts.get_reply!(id)
    {:ok, _reply} = Posts.delete_reply(reply)

    conn
    |> put_flash(:info, "Reply deleted successfully.")
    |> redirect(to: Routes.course_post_reply_path(conn, :index, course_id, post_id))
  end
end
