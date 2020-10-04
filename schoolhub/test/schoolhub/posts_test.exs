defmodule Schoolhub.PostsTest do
  use Schoolhub.DataCase

  alias Schoolhub.{Accounts, Courses, Posts}

  describe "posts" do
    alias Schoolhub.Posts.Post

    @valid_user_attrs %{email: "some email",
			name: "some name",
			credential: %{username: "some username",
				      password: "some password"}}
    @valid_course_attrs %{description: "some description", name: "some name"}
     
    @valid_attrs %{content: "some content", pinned: false}
    @update_attrs %{content: "some updated content", pinned: true}
    @invalid_attrs %{content: nil, pinned: nil}

    def ids_fixture() do
      {:ok, _user = %{id: user_id}} =
        %{}
        |> Enum.into(@valid_user_attrs)
        |> Accounts.create_user()
      
      {:ok, _course = %{id: course_id}} =
        %{}
        |> Enum.into(%{creator: user_id})
        |> Enum.into(@valid_course_attrs)
        |> Courses.create_course()

      %{course_id: course_id, creator: user_id}
    end
    
    def post_fixture(attrs \\ %{}) do
      {:ok, post} =
        attrs
	|> Enum.into(ids_fixture())
        |> Enum.into(@valid_attrs)
        |> Posts.create_post()

      post
      |> Repo.preload(:reply)
    end

    test "list_posts/0 returns some posts" do
      post = post_fixture()
      assert Posts.list_posts() == [post]
    end
    
    test "list_posts/1 returns some posts" do
      post = post_fixture()
      assert Posts.list_posts("5") == [post]
    end

    test "get_post!/1 returns the post with given id" do
      post = post_fixture()
      assert Posts.get_post!(post.id) == post
    end

    test "create_post/1 with valid data creates a post" do
      attrs = ids_fixture()
      |> Enum.into(@valid_attrs)
      
      assert {:ok, %Post{} = post} = Posts.create_post(attrs)
      assert post.content == "some content"
      assert post.pinned == false
    end

    test "create_post/1 with invalid data returns error changeset" do
      assert {:error, %Ecto.Changeset{}} = Posts.create_post(@invalid_attrs)
    end

    test "update_post/2 with valid data updates the post" do
      post = post_fixture()
      assert {:ok, %Post{} = post} = Posts.update_post(post, @update_attrs)
      assert post.content == "some updated content"
      assert post.pinned == true
    end

    test "update_post/2 with invalid data returns error changeset" do
      post = post_fixture()
      assert {:error, %Ecto.Changeset{}} = Posts.update_post(post, @invalid_attrs)
      assert post == Posts.get_post!(post.id)
    end

    test "delete_post/1 deletes the post" do
      post = post_fixture()
      assert {:ok, %Post{}} = Posts.delete_post(post)
      assert_raise Ecto.NoResultsError, fn -> Posts.get_post!(post.id) end
    end

    test "change_post/1 returns a post changeset" do
      post = post_fixture()
      assert %Ecto.Changeset{} = Posts.change_post(post)
    end
  end

  describe "post_replies" do
    alias Schoolhub.Posts.Reply

    @valid_user_attrs %{email: "some email",
			name: "some name",
			credential: %{username: "some username",
				      password: "some password"}}
    @valid_course_attrs %{description: "some description", name: "some name"}

    @valid_attrs %{content: "some content"}
    @update_attrs %{content: "some updated content"}
    @invalid_attrs %{content: nil}

    def ids_fixture2() do
      {:ok, _user = %{id: user_id}} =
        %{}
        |> Enum.into(@valid_user_attrs)
        |> Accounts.create_user()
      
      {:ok, _course = %{id: course_id}} =
        %{}
        |> Enum.into(%{creator: user_id})
        |> Enum.into(@valid_course_attrs)
        |> Courses.create_course()

      {:ok, _post = %{id: post_id}} =
        @valid_attrs
	|> Enum.into(%{creator: user_id, course_id: course_id})
        |> Posts.create_post()

      %{parent_post: post_id, creator: user_id}
    end

    def reply_fixture(attrs \\ %{}) do
      {:ok, reply} =
        attrs
	|> Enum.into(ids_fixture2())
        |> Enum.into(@valid_attrs)
        |> Posts.create_reply()

      reply
    end

    test "list_post_replies/0 returns all post_replies" do
      reply = reply_fixture()
      assert Posts.list_post_replies() == [reply]
    end

    test "get_reply!/1 returns the reply with given id" do
      reply = reply_fixture()
      assert Posts.get_reply!(reply.id) == reply
    end

    test "create_reply/1 with valid data creates a reply" do
      attrs = ids_fixture2()
      |> Enum.into(@valid_attrs)
      
      assert {:ok, %Reply{} = reply} = Posts.create_reply(attrs)
      assert reply.content == "some content"
    end

    test "create_reply/1 with invalid data returns error changeset" do
      assert {:error, %Ecto.Changeset{}} = Posts.create_reply(@invalid_attrs)
    end

    test "update_reply/2 with valid data updates the reply" do
      reply = reply_fixture()
      assert {:ok, %Reply{} = reply} = Posts.update_reply(reply, @update_attrs)
      assert reply.content == "some updated content"
    end

    test "update_reply/2 with invalid data returns error changeset" do
      reply = reply_fixture()
      assert {:error, %Ecto.Changeset{}} = Posts.update_reply(reply, @invalid_attrs)
      assert reply == Posts.get_reply!(reply.id)
    end

    test "delete_reply/1 deletes the reply" do
      reply = reply_fixture()
      assert {:ok, %Reply{}} = Posts.delete_reply(reply)
      assert_raise Ecto.NoResultsError, fn -> Posts.get_reply!(reply.id) end
    end

    test "change_reply/1 returns a reply changeset" do
      reply = reply_fixture()
      assert %Ecto.Changeset{} = Posts.change_reply(reply)
    end
  end
end
