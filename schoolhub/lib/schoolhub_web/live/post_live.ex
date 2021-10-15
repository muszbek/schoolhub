defmodule SchoolhubWeb.PostLive do
  use Phoenix.LiveView

  alias Schoolhub.Posts
  
  @posts_increment 5
  
  def mount(_params, %{"internal_host" => host, "course_id" => course_id, "limit" => limit},
    socket) do
    
    posts = Posts.list_course_posts(course_id, limit)
    limit = if Enum.count(posts) < String.to_integer(limit), do: -1, else: limit
    assigns = [host: host, posts: posts, course_id: course_id, post_limit: limit]
    {:ok, assign(socket, assigns)}
  end

  def render(assigns) do
    SchoolhubWeb.PostView.render("index.html", assigns)
  end

  def handle_event("more_clicked", _value, socket) do
    assigns = socket.assigns
    new_limit = String.to_integer(assigns.post_limit) + @posts_increment
    posts = Posts.list_course_posts(assigns.course_id, new_limit)
    limit = if Enum.count(posts) < new_limit, do: -1, else: new_limit
    assigns = [posts: posts, post_limit: limit]
    {:noreply, assign(socket, assigns)}
  end
end
