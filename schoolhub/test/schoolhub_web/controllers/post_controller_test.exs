defmodule SchoolhubWeb.PostControllerTest do
  use SchoolhubWeb.ConnCase

  alias Schoolhub.{Accounts, Courses, Privileges, Posts}

  @create_attrs %{content: "some content", pinned: false}
  @update_attrs %{content: "some updated content", pinned: false}
  @invalid_attrs %{content: nil, pinned: nil}

  @create_user_attrs %{email: "some email",
		       name: "some name",
		       credential: %{username: "some username",
				     password: "some password"}}
  @create_course_attrs %{description: "some description", name: "some name"}

  def fixture(:course, conn = %Plug.Conn{}) do
    {:ok, user} = Accounts.create_user(@create_user_attrs)
    Privileges.update_privilege(user.privilege, %{level: "admin"})
    
    new_conn = conn
    |> Plug.Test.init_test_session(user_id: nil)
    |> SchoolhubWeb.SessionController.enter_session(user)

    {:ok, _course = %{id: course_id}} = Courses.create_course(@create_course_attrs)
    aff_attrs = %{course_id: course_id, user_id: user.id, affiliation: "owner"}
    {:ok, _aff} = Courses.create_affiliation(aff_attrs)

    %{conn: new_conn, course_id: course_id, creator: user.id}
  end
  
  def fixture(:post, course_id, creator) do
    post_attrs = create_valid_attrs(@create_attrs, course_id, creator)
    {:ok, post} = Posts.create_post(post_attrs)
    post
  end

  
  describe "index" do
    setup [:create_course]
    
    test "lists all posts", %{conn: conn, course_id: course_id} do
      conn = get(conn, Routes.course_post_path(conn, :index, course_id))
      assert html_response(conn, 200) =~ "Listing Posts"
    end
  end

  describe "new post" do
    setup [:create_course]
    
    test "renders form", %{conn: conn, course_id: course_id} do
      conn = get(conn, Routes.course_post_path(conn, :new, course_id))
      assert html_response(conn, 200) =~ "New Post"
    end
  end

  describe "create post" do
    setup [:create_course]
    
    test "redirects to show when data is valid", %{conn: conn, course_id: course_id} do
      creator = get_session(conn, :user_id)
      post_attrs = create_valid_attrs(@create_attrs, course_id, creator)
      conn = post(conn, Routes.course_post_path(conn, :create, course_id), post: post_attrs)

      assert %{id: id} = redirected_params(conn)
      assert redirected_to(conn) == Routes.course_post_path(conn, :show, course_id, id)

      conn = get(conn, Routes.course_post_path(conn, :show, course_id, id))
      assert html_response(conn, 200) =~ "Show Post"
    end

    test "renders errors when data is invalid", %{conn: conn, course_id: course_id} do
      conn = post(conn, Routes.course_post_path(conn, :create, course_id), post: @invalid_attrs)
      assert html_response(conn, 200) =~ "New Post"
    end
  end

  describe "edit post" do
    setup [:create_course]
    setup [:create_post]

    test "renders form for editing chosen post", %{conn: conn, course_id: course_id, post: post} do
      conn = get(conn, Routes.course_post_path(conn, :edit, course_id, post))
      assert html_response(conn, 200) =~ "Edit Post"
    end
  end

  describe "update post" do
    setup [:create_course]
    setup [:create_post]

    test "redirects when data is valid", %{conn: conn, course_id: course_id, post: post} do
      conn = put(conn, Routes.course_post_path(conn, :update, course_id, post), post: @update_attrs)
      assert redirected_to(conn) == Routes.course_post_path(conn, :show, course_id, post)

      conn = get(conn, Routes.course_post_path(conn, :show, course_id, post))
      assert html_response(conn, 200) =~ "some updated content"
    end

    test "renders errors when data is invalid", %{conn: conn, course_id: course_id, post: post} do
      conn = put(conn, Routes.course_post_path(conn, :update, course_id, post), post: @invalid_attrs)
      assert html_response(conn, 200) =~ "Edit Post"
    end
  end

  describe "delete post" do
    setup [:create_course]
    setup [:create_post]

    test "deletes chosen post", %{conn: conn, course_id: course_id, post: post} do
      conn = delete(conn, Routes.course_post_path(conn, :delete, course_id, post))
      assert redirected_to(conn) == Routes.course_post_path(conn, :index, course_id)
      assert_error_sent 404, fn ->
        get(conn, Routes.course_post_path(conn, :show, course_id, post))
      end
    end
  end

  describe "pin post" do
    setup [:create_course]
    setup [:create_post]

    test "pins post", %{conn: conn, course_id: course_id, post: post} do
      conn = put(conn, Routes.course_post_post_path(conn, :pin, course_id, post), to_pin: true)
      assert redirected_to(conn) == Routes.course_post_path(conn, :index, course_id)

      conn = get(conn, Routes.course_post_path(conn, :show, course_id, post))
      assert html_response(conn, 200) =~ "true"
    end

    test "unpins post", %{conn: conn, course_id: course_id, post: post} do
      conn = put(conn, Routes.course_post_post_path(conn, :pin, course_id, post), to_pin: false)
      assert redirected_to(conn) == Routes.course_post_path(conn, :index, course_id)

      conn = get(conn, Routes.course_post_path(conn, :show, course_id, post))
      assert html_response(conn, 200) =~ "false"
    end
  end

  
  defp create_post(%{course_id: course_id, creator: creator}) do
    post = fixture(:post, course_id, creator)
    %{post: post}
  end

  defp create_course(%{conn: conn}) do
    _conn_and_ids = fixture(:course, conn)
  end


  defp create_valid_attrs(attrs, course_id, creator) do
    attrs
    |> Map.put("course_id", course_id)
    |> Map.put("creator", creator)
    |> Morphix.atomorphify!()
  end
end
