defmodule SchoolhubWeb.PostController do
  use SchoolhubWeb, :controller

  alias Schoolhub.Posts
  alias Schoolhub.Posts.Post
  
  @post_limit_default "5"

  def index(conn, %{"course_id" => course_id, "limit" => limit}) do
    posts = Posts.list_course_posts(course_id, limit)
    limit = if Enum.count(posts) < String.to_integer(limit), do: -1, else: limit
    render(conn, "index.html", posts: posts, course_id: course_id, post_limit: limit)
  end
  def index(conn, %{"course_id" => course_id}) do
    index(conn, %{"course_id" => course_id, "limit" => @post_limit_default})
  end

  def new(conn, %{"course_id" => course_id}) do
    changeset = Posts.change_post(%Post{})
    render(conn, "new.html", changeset: changeset, course_id: course_id)
  end

  def create(conn, %{"course_id" => course_id, "post" => post_params}) do
    user_id = get_session(conn, :user_id)
    post_params_with_creator = post_params
    |> Map.put("creator", user_id)
    |> Map.put("pinned", false)
    |> Morphix.atomorphify!()
    
    case Posts.create_post(post_params_with_creator) do
      {:ok, post} ->
        conn
        |> put_flash(:info, "Post created successfully.")
        |> redirect(to: Routes.course_post_path(conn, :show, course_id, post))

      {:error, %Ecto.Changeset{} = changeset} ->
        render(conn, "new.html", changeset: changeset, course_id: course_id)
    end
  end

  def show(conn, %{"course_id" => course_id, "id" => id}) do
    post = Posts.get_post!(id)
    render(conn, "show.html", post: post, course_id: course_id, replies: post.reply)
  end

  def edit(conn, %{"course_id" => course_id, "id" => id}) do
    post = Posts.get_post!(id)
    changeset = Posts.change_post(post)
    render(conn, "edit.html", post: post, changeset: changeset, course_id: course_id)
  end

  def update(conn, %{"course_id" => course_id, "id" => id, "post" => post_params}) do
    post = Posts.get_post!(id)

    case Posts.update_post(post, post_params) do
      {:ok, post} ->
        conn
        |> put_flash(:info, "Post updated successfully.")
        |> redirect(to: Routes.course_post_path(conn, :show, course_id, post))

      {:error, %Ecto.Changeset{} = changeset} ->
        render(conn, "edit.html", post: post, changeset: changeset, course_id: course_id)
    end
  end

  def delete(conn, %{"course_id" => course_id, "id" => id}) do
    post = Posts.get_post!(id)
    {:ok, _post} = Posts.delete_post(post)

    conn
    |> put_flash(:info, "Post deleted successfully.")
    |> redirect(to: Routes.course_post_path(conn, :index, course_id))
  end

  def pin(conn, %{"course_id" => course_id, "post_id" => id, "to_pin" => to_pin}) do
    
    post = Posts.get_post!(id)
    {:ok, _post} = Posts.update_post(post, %{pinned: to_pin})
    
    msg = if to_pin == "true", do: "Post pinned.", else: "Post unpinned."
    
    conn
    |> put_flash(:info, msg)
    |> redirect(to: Routes.course_post_path(conn, :index, course_id))
  end
    
end
