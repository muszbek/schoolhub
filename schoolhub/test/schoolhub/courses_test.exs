defmodule Schoolhub.CoursesTest do
  use Schoolhub.DataCase

  alias Schoolhub.Courses

  describe "courses" do
    alias Schoolhub.Courses.Course

    @valid_attrs %{description: "some description", name: "some name"}
    @update_attrs %{description: "some updated description", name: "some updated name"}
    @invalid_attrs %{description: nil, name: nil}

    def course_fixture(attrs \\ %{}) do
      {:ok, course} =
        attrs
        |> Enum.into(@valid_attrs)
        |> Courses.create_course()

      course
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
      assert {:ok, %Course{} = course} = Courses.create_course(@valid_attrs)
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
  end

  describe "course_affiliations" do
    alias Schoolhub.Courses.Affiliation

    @valid_attrs %{affiliation: "some affiliation"}
    @update_attrs %{affiliation: "some updated affiliation"}
    @invalid_attrs %{affiliation: nil}

    def affiliation_fixture(attrs \\ %{}) do
      {:ok, affiliation} =
        attrs
        |> Enum.into(@valid_attrs)
        |> Courses.create_affiliation()

      affiliation
    end

    test "list_course_affiliations/0 returns all course_affiliations" do
      affiliation = affiliation_fixture()
      assert Courses.list_course_affiliations() == [affiliation]
    end

    test "get_affiliation!/1 returns the affiliation with given id" do
      affiliation = affiliation_fixture()
      assert Courses.get_affiliation!(affiliation.id) == affiliation
    end

    test "create_affiliation/1 with valid data creates a affiliation" do
      assert {:ok, %Affiliation{} = affiliation} = Courses.create_affiliation(@valid_attrs)
      assert affiliation.affiliation == "some affiliation"
    end

    test "create_affiliation/1 with invalid data returns error changeset" do
      assert {:error, %Ecto.Changeset{}} = Courses.create_affiliation(@invalid_attrs)
    end

    test "update_affiliation/2 with valid data updates the affiliation" do
      affiliation = affiliation_fixture()
      assert {:ok, %Affiliation{} = affiliation} = Courses.update_affiliation(affiliation, @update_attrs)
      assert affiliation.affiliation == "some updated affiliation"
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
