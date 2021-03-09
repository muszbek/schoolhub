defmodule Schoolhub.QuestionsTest do
  use Schoolhub.DataCase

  alias Schoolhub.{Accounts, Courses, Questions}

  describe "questions" do
    alias Schoolhub.Questions.Question

    @valid_user_attrs %{email: "some email",
			name: "some name",
			credential: %{username: "some username",
				      password: "some password"}}
    @valid_course_attrs %{description: "some description", name: "some name"}
    
    @valid_attrs %{content: "some content", pinned: true, tags: ["some tag"]}
    @update_attrs %{content: "some updated content", pinned: false, tags: []}
    @invalid_attrs %{content: nil, pinned: nil, tags: nil}

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
    
    def question_fixture(attrs \\ %{}) do
      {:ok, question} =
        attrs
	|> Enum.into(ids_fixture())
        |> Enum.into(@valid_attrs)
        |> Questions.create_question()

	question
	|> Repo.preload(:qreply)
    end

    test "list_course_questions/0 returns some questions" do
      question = question_fixture()
      assert Questions.list_course_questions(question.course_id) == [question]
    end

    test "list_course_questions/1 returns some questions" do
      question = question_fixture()
      assert Questions.list_course_questions(question.course_id, "5") == [question]
    end

    test "filter_questions/2 with right filters returns questions" do
      question = question_fixture()
      assert Questions.filter_questions(question.course_id, ["some tag"]) == [question]
    end

    test "filter questions/2 with wrong filters returns nothing" do
      question = question_fixture()
      assert Questions.filter_questions(question.course_id, ["invalid tag"]) == []
    end

    test "filter questions/2 with no filters returns all questions" do
      question = question_fixture()
      assert Questions.filter_questions(question.course_id, []) == [question]
    end

    test "get_question!/1 returns the question with given id" do
      question = question_fixture()
      assert Questions.get_question!(question.id) == question
    end

    test "create_question/1 with valid data creates a question" do
      attrs = ids_fixture()
      |> Enum.into(@valid_attrs)
      
      assert {:ok, %Question{} = question} = Questions.create_question(attrs)
      assert question.content == "some content"
      assert question.pinned == true
      assert question.tags == ["some tag"]
    end

    test "create_question/1 with invalid data returns error changeset" do
      assert {:error, %Ecto.Changeset{}} = Questions.create_question(@invalid_attrs)
    end

    test "update_question/2 with valid data updates the question" do
      question = question_fixture()
      assert {:ok, %Question{} = question} = Questions.update_question(question, @update_attrs)
      assert question.content == "some updated content"
      assert question.pinned == false
      assert question.tags == []
    end

    test "update_question/2 with invalid data returns error changeset" do
      question = question_fixture()
      assert {:error, %Ecto.Changeset{}} = Questions.update_question(question, @invalid_attrs)
      assert question == Questions.get_question!(question.id)
    end

    test "delete_question/1 deletes the question" do
      question = question_fixture()
      assert {:ok, %Question{}} = Questions.delete_question(question)
      assert_raise Ecto.NoResultsError, fn -> Questions.get_question!(question.id) end
    end

    test "change_question/1 returns a question changeset" do
      question = question_fixture()
      assert %Ecto.Changeset{} = Questions.change_question(question)
    end
  end

  describe "question_replies" do
    alias Schoolhub.Questions.Qreply

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

      {:ok, _question = %{id: question_id}} =
        @valid_attrs
	|> Enum.into(%{creator: user_id, course_id: course_id})
        |> Questions.create_question()

      %{parent_question: question_id, creator: user_id}
    end
    
    def qreply_fixture(attrs \\ %{}) do
      {:ok, qreply} =
        attrs
	|> Enum.into(ids_fixture2())
        |> Enum.into(@valid_attrs)
        |> Questions.create_qreply()

      qreply
    end

    test "list_question_replies/0 returns all question_replies" do
      qreply = qreply_fixture()
      assert Questions.list_question_replies() == [qreply]
    end

    test "get_qreply!/1 returns the qreply with given id" do
      qreply = qreply_fixture()
      assert Questions.get_qreply!(qreply.id) == qreply
    end

    test "create_qreply/1 with valid data creates a qreply" do
      attrs = ids_fixture2()
      |> Enum.into(@valid_attrs)
      
      assert {:ok, %Qreply{} = qreply} = Questions.create_qreply(attrs)
      assert qreply.content == "some content"
    end

    test "create_qreply/1 with invalid data returns error changeset" do
      assert {:error, %Ecto.Changeset{}} = Questions.create_qreply(@invalid_attrs)
    end

    test "update_qreply/2 with valid data updates the qreply" do
      qreply = qreply_fixture()
      assert {:ok, %Qreply{} = qreply} = Questions.update_qreply(qreply, @update_attrs)
      assert qreply.content == "some updated content"
    end

    test "update_qreply/2 with invalid data returns error changeset" do
      qreply = qreply_fixture()
      assert {:error, %Ecto.Changeset{}} = Questions.update_qreply(qreply, @invalid_attrs)
      assert qreply == Questions.get_qreply!(qreply.id)
    end

    test "delete_qreply/1 deletes the qreply" do
      qreply = qreply_fixture()
      assert {:ok, %Qreply{}} = Questions.delete_qreply(qreply)
      assert_raise Ecto.NoResultsError, fn -> Questions.get_qreply!(qreply.id) end
    end

    test "change_qreply/1 returns a qreply changeset" do
      qreply = qreply_fixture()
      assert %Ecto.Changeset{} = Questions.change_qreply(qreply)
    end
  end
end
