defmodule Schoolhub.GradesTest do
  use Schoolhub.DataCase

  alias Schoolhub.{Accounts, Courses, Grades}

  describe "grades" do
    alias Schoolhub.Grades.Grade

    @valid_user_attrs %{email: "some email",
			name: "some name",
			credential: %{username: "some username",
				      password: "some password"}}
    @valid_course_attrs %{description: "some description", name: "some name"}
    
    @valid_attrs %{grades: %{}}
    @update_attrs %{grades: %{}}
    @invalid_attrs %{grades: nil}
    @add_attrs %{title: "grade"}

    def affiliation_fixture() do
      {:ok, _user = %{id: user_id}} =
        %{}
        |> Enum.into(@valid_user_attrs)
        |> Accounts.create_user()
      
      {:ok, _course = %{id: course_id}} =
        %{}
        |> Enum.into(%{creator: user_id})
        |> Enum.into(@valid_course_attrs)
        |> Courses.create_course()

      {:ok, affiliation} =
        %{course_id: course_id, user_id: user_id}
        |> Enum.into(@valid_attrs)
        |> Courses.create_affiliation()

      %{affiliation_id: affiliation.id}
    end
    
    def grade_fixture(attrs \\ %{}) do
      {:ok, grade} =
        attrs
	|> Enum.into(affiliation_fixture())
        |> Enum.into(@valid_attrs)
        |> Grades.create_grade()

      grade
    end

    test "list_grades/0 returns all grades" do
      %{affiliation_id: aff_id} = affiliation_fixture()
      %{grade: grade} = Courses.get_affiliation!(aff_id)
      assert Grades.list_grades() == [grade]
    end

    test "get_grade!/1 returns the grade with given id" do
      grade = grade_fixture()
      assert Grades.get_grade!(grade.id) == grade
    end

    test "get_grade_by_aff!/1 returns the grade with given id" do
      %{affiliation_id: aff_id} = affiliation_fixture()
      %{grade: grade} = Courses.get_affiliation!(aff_id)
      assert Grades.get_grade_by_aff!(aff_id) == grade
    end

    test "create_grade/1 with valid data creates a grade" do
      valid_attrs_with_aff = Enum.into(affiliation_fixture(), @valid_attrs)
      assert {:ok, %Grade{} = grade} = Grades.create_grade(valid_attrs_with_aff)
      assert grade.grades == %{}
    end

    test "create_grade/1 with invalid data returns error changeset" do
      assert {:error, %Ecto.Changeset{}} = Grades.create_grade(@invalid_attrs)
    end

    test "add_grade/2 appends to grade map" do
      grade = grade_fixture()
      assert {:ok, %Grade{} = new_grade} = Grades.add_grade(grade.id, @add_attrs)
      assert new_grade.grades == @add_attrs
    end

    test "update_grade/2 with valid data updates the grade" do
      grade = grade_fixture()
      assert {:ok, %Grade{} = grade} = Grades.update_grade(grade, @update_attrs)
      assert grade.grades == %{}
    end

    test "update_grade/2 with invalid data returns error changeset" do
      grade = grade_fixture()
      assert {:error, %Ecto.Changeset{}} = Grades.update_grade(grade, @invalid_attrs)
      assert grade == Grades.get_grade!(grade.id)
    end

    test "delete_grade/1 deletes the grade" do
      grade = grade_fixture()
      assert {:ok, %Grade{}} = Grades.delete_grade(grade)
      assert_raise Ecto.NoResultsError, fn -> Grades.get_grade!(grade.id) end
    end

    test "change_grade/1 returns a grade changeset" do
      grade = grade_fixture()
      assert %Ecto.Changeset{} = Grades.change_grade(grade)
    end
  end
end
