defmodule SchoolhubWeb.ReplyControllerTest do
  use SchoolhubWeb.ConnCase

  alias Schoolhub.{Accounts, Courses, Privileges, Posts}

  @create_attrs %{content: "some content"}
  @update_attrs %{content: "some updated content"}
  @invalid_attrs %{content: nil}

  @create_user_attrs %{email: "some email",
		       name: "some name",
		       credential: %{username: "some username",
				     password: "some password"}}
  @create_course_attrs %{description: "some description", name: "some name"}
  @create_post_attrs %{content: "some post content", pinned: false}

  def fixture(:post, conn = %Plug.Conn{}) do
    {:ok, user} = Accounts.create_user(@create_user_attrs)
    Privileges.update_privilege(user.privilege, %{level: "admin"})
    
    new_conn = conn
    |> Plug.Test.init_test_session(user_id: nil)
    |> SchoolhubWeb.SessionController.enter_session(user)

    {:ok, _course = %{id: course_id}} = Courses.create_course(@create_course_attrs)
    aff_attrs = %{course_id: course_id, user_id: user.id, affiliation: "owner"}
    {:ok, _aff} = Courses.create_affiliation(aff_attrs)

    {:ok, post} = @create_post_attrs
    |> Map.put("course_id", course_id)
    |> Map.put("creator", user.id)
    |> Morphix.atomorphify!()
    |> Posts.create_post()

    %{conn: new_conn, course_id: course_id, parent_post: post.id, creator: user.id}
  end
  
  def fixture(:reply, post_id, creator) do
    reply_attrs = create_valid_attrs(@create_attrs, post_id, creator)
    {:ok, reply} = Posts.create_reply(reply_attrs)
    reply
  end
  

  describe "index" do
    setup [:create_post]
    
    test "lists all post_replies", %{conn: conn, course_id: course_id, parent_post: post_id} do
      conn = get(conn, Routes.course_post_reply_path(conn, :index, course_id, post_id))
      assert redirected_to(conn) == Routes.course_post_path(conn, :show,
	course_id, post_id)
    end
  end

  describe "new reply" do
    setup [:create_post]
    
    test "renders form", %{conn: conn, course_id: course_id, parent_post: post_id} do
      conn = get(conn, Routes.course_post_reply_path(conn, :new, course_id, post_id))
      assert html_response(conn, 200) =~ "New Reply"
    end
  end

  describe "create reply" do
    setup [:create_post]
    
    test "redirects to show when data is valid", %{conn: conn,
						   course_id: course_id, parent_post: post_id} do
      conn = post(conn, Routes.course_post_reply_path(conn, :create, course_id, post_id),
	reply: @create_attrs)

      assert %{id: id} = redirected_params(conn)
      assert redirected_to(conn) == Routes.course_post_reply_path(conn, :show,
	course_id, post_id, id)

      conn = get(conn, Routes.course_post_reply_path(conn, :show, course_id, post_id, id))
      assert html_response(conn, 200) =~ "Show Reply"
    end

    test "renders errors when data is invalid", %{conn: conn,
						  course_id: course_id, parent_post: post_id} do
      conn = post(conn, Routes.course_post_reply_path(conn, :create, course_id, post_id),
	reply: @invalid_attrs)
      assert html_response(conn, 200) =~ "New Reply"
    end
  end

  describe "edit reply" do
    setup [:create_post]
    setup [:create_reply]

    test "renders form for editing chosen reply", %{conn: conn, course_id: course_id,
						    parent_post: post_id, reply: reply} do
      conn = get(conn, Routes.course_post_reply_path(conn, :edit, course_id, post_id, reply))
      assert html_response(conn, 200) =~ "Edit Reply"
    end
  end

  describe "update reply" do
    setup [:create_post]
    setup [:create_reply]

    test "redirects when data is valid", %{conn: conn, course_id: course_id,
					   parent_post: post_id, reply: reply} do
      conn = put(conn, Routes.course_post_reply_path(conn, :update, course_id, post_id, reply),
	reply: @update_attrs)
      assert redirected_to(conn) == Routes.course_post_reply_path(conn, :show,
	course_id, post_id, reply)

      conn = get(conn, Routes.course_post_reply_path(conn, :show, course_id, post_id, reply))
      assert html_response(conn, 200) =~ "some updated content"
    end

    test "renders errors when data is invalid", %{conn: conn, course_id: course_id,
						  parent_post: post_id, reply: reply} do
      conn = put(conn, Routes.course_post_reply_path(conn, :update, course_id, post_id, reply),
	reply: @invalid_attrs)
      assert html_response(conn, 200) =~ "Edit Reply"
    end
  end

  describe "delete reply" do
    setup [:create_post]
    setup [:create_reply]

    test "deletes chosen reply", %{conn: conn, course_id: course_id,
				   parent_post: post_id, reply: reply} do
      conn = delete(conn, Routes.course_post_reply_path(conn, :delete, course_id, post_id, reply))
      assert redirected_to(conn) == Routes.course_post_reply_path(conn, :index, course_id, post_id)
      assert_error_sent 404, fn ->
        get(conn, Routes.course_post_reply_path(conn, :show, course_id, post_id, reply))
      end
    end
  end

  
  defp create_reply(%{parent_post: post_id, creator: creator}) do
    reply = fixture(:reply, post_id, creator)
    %{reply: reply}
  end

  defp create_post(%{conn: conn}) do
    _conn_and_ids = fixture(:post, conn)
  end


  defp create_valid_attrs(attrs, post_id, creator) do
    attrs
    |> Map.put("parent_post", post_id)
    |> Map.put("creator", creator)
    |> Morphix.atomorphify!()
  end
end
