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
end
