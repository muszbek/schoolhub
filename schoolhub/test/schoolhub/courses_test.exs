defmodule Schoolhub.CoursesTest do
  use Schoolhub.DataCase

  alias Schoolhub.Repo
  alias Schoolhub.Courses
  alias Schoolhub.Accounts

  describe "courses" do
    alias Schoolhub.Courses.Course

    @valid_user_attrs %{email: "some email",
			name: "some name",
			credential: %{username: "some username",
				      password: "some password"}}
    
    @valid_attrs %{description: "some description", name: "some name"}
    @update_attrs %{description: "some updated description", name: "some updated name"}
    @invalid_attrs %{description: nil, name: nil}

    def user_fixture() do
      {:ok, user} =
        %{}
        |> Enum.into(@valid_user_attrs)
        |> Accounts.create_user()
      user
    end
    
    def course_fixture(attrs \\ %{}) do
      _user = %{id: user_id} = user_fixture()
      
      {:ok, course} =
        attrs
        |> Enum.into(%{creator: user_id})
        |> Enum.into(@valid_attrs)
        |> Courses.create_course()

      course
      |> Repo.preload(:affiliation)
      |> Repo.preload(:post)
    end

    test "list_courses/0 returns all courses" do
      course = course_fixture()
      assert Courses.list_courses() == [course]
    end

    test "get_course!/1 returns the course with given id" do
      course = course_fixture()
      assert Courses.get_course!(course.id) == course
    end

    test "create_course/1 with valid data creates a course" do
      user = user_fixture()
      attrs = create_valid_attrs(@valid_attrs, user)
      assert {:ok, %Course{} = course} = Courses.create_course(attrs)
      assert course.description == "some description"
      assert course.name == "some name"
    end

    test "create_course/1 with invalid data returns error changeset" do
      assert {:error, %Ecto.Changeset{}} = Courses.create_course(@invalid_attrs)
    end

    test "update_course/2 with valid data updates the course" do
      course = course_fixture()
      assert {:ok, %Course{} = course} = Courses.update_course(course, @update_attrs)
      assert course.description == "some updated description"
      assert course.name == "some updated name"
    end

    test "update_course/2 with invalid data returns error changeset" do
      course = course_fixture()
      assert {:error, %Ecto.Changeset{}} = Courses.update_course(course, @invalid_attrs)
      assert course == Courses.get_course!(course.id)
    end

    test "delete_course/1 deletes the course" do
      course = course_fixture()
      assert {:ok, %Course{}} = Courses.delete_course(course)
      assert_raise Ecto.NoResultsError, fn -> Courses.get_course!(course.id) end
    end

    test "change_course/1 returns a course changeset" do
      course = course_fixture()
      assert %Ecto.Changeset{} = Courses.change_course(course)
    end

    
    defp create_valid_attrs(attrs, _user = %{id: user_id}) do
      attrs
      |> Enum.into(%{creator: user_id})
      |> Enum.into(@valid_attrs)
    end
  end

  describe "course_affiliations" do
    alias Schoolhub.Courses.Affiliation

    @valid_user_attrs %{email: "some email",
			name: "some name",
			credential: %{username: "some username",
				      password: "some password"}}
    @valid_course_attrs %{description: "some description", name: "some name"}

    @valid_attrs %{affiliation: "student"}
    @update_attrs %{affiliation: "assistant"}
    @owner_attrs %{affiliation: "owner"}
    @invalid_attrs %{affiliation: "some invalid affiliation"}

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

      %{course_id: course_id, user_id: user_id}
    end
    
    def affiliation_fixture(attrs \\ %{}) do
      {:ok, affiliation} =
        attrs
        |> Enum.into(ids_fixture())
        |> Enum.into(@valid_attrs)
        |> Courses.create_affiliation()

      affiliation
    end

    test "list_course_affiliations/0 returns all course_affiliations" do
      aff = affiliation_fixture()
      assert Courses.list_course_affiliations(aff.course_id) == [aff]
    end

    test "get_username!/2 returns only name when student" do
      _aff = %{course_id: course_id, user_id: user_id, affiliation: "student"} =
	affiliation_fixture()
      user = Accounts.get_user!(user_id)
      assert Courses.get_username!(user_id, course_id) == user.name
    end

    test "get_username!/2 returns extended name when not student" do
      {:ok, %Affiliation{course_id: course_id, user_id: user_id, affiliation: "assistant"}} =
	Courses.update_affiliation(affiliation_fixture(), @update_attrs)
      user = Accounts.get_user!(user_id)
      assert Courses.get_username!(user_id, course_id) == user.name <> " (assistant)"
    end

    test "list_affiliated_courses/1 returns all courses that user is member of" do
      _aff = %{course_id: course_id, user_id: user_id} = affiliation_fixture()
      course = Courses.get_course!(course_id)
      assert Courses.list_affiliated_courses(user_id) == [course]
    end

    test "get_affiliation!/1 returns the affiliation with given id" do
      affiliation = affiliation_fixture()
      assert Courses.get_affiliation!(affiliation.id) == affiliation
    end

    test "get_owner!/1 returns the affiliation that owns given course" do
      affiliation = affiliation_fixture()
      assert {:ok, %Affiliation{} = affiliation} = Courses.update_affiliation(affiliation, @owner_attrs)
      assert affiliation.affiliation == "owner"

      assert Courses.get_owner!(affiliation.course_id) == affiliation
    end

    test "get_affiliation_by_user!/2 returns the affiliation with given id" do
      affiliation = affiliation_fixture()
      user_id = affiliation.user_id
      course_id = affiliation.course_id

      assert Courses.get_affiliation_by_user!(course_id, user_id) == affiliation
    end

    test "create_affiliation/1 with valid data creates a affiliation" do
      attrs = ids_fixture()
      |> Enum.into(@valid_attrs)
      
      assert {:ok, %Affiliation{} = affiliation} = Courses.create_affiliation(attrs)
      assert affiliation.affiliation == "student"
    end

    test "create_affiliation/1 with invalid data returns error changeset" do
      assert {:error, %Ecto.Changeset{}} = Courses.create_affiliation(@invalid_attrs)
    end

    test "update_affiliation/2 with valid data updates the affiliation" do
      affiliation = affiliation_fixture()
      assert {:ok, %Affiliation{} = affiliation} = Courses.update_affiliation(affiliation, @update_attrs)
      assert affiliation.affiliation == "assistant"
    end

    test "update_affiliation/2 with invalid data returns error changeset" do
      affiliation = affiliation_fixture()
      assert {:error, %Ecto.Changeset{}} = Courses.update_affiliation(affiliation, @invalid_attrs)
      assert affiliation == Courses.get_affiliation!(affiliation.id)
    end

    test "delete_affiliation/1 deletes the affiliation" do
      affiliation = affiliation_fixture()
      assert {:ok, %Affiliation{}} = Courses.delete_affiliation(affiliation)
      assert_raise Ecto.NoResultsError, fn -> Courses.get_affiliation!(affiliation.id) end
    end

    test "change_affiliation/1 returns a affiliation changeset" do
      affiliation = affiliation_fixture()
      assert %Ecto.Changeset{} = Courses.change_affiliation(affiliation)
    end
  end
end
