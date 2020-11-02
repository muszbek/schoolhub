defmodule Schoolhub.FilesTest do
  use Schoolhub.DataCase

  alias Schoolhub.{Accounts, Courses, Files}

  describe "files" do
    alias Schoolhub.Files.File

    @valid_user_attrs %{email: "some email",
			name: "some name",
			credential: %{username: "some username",
				      password: "some password"}}
    @valid_course_attrs %{description: "some description", name: "some name"}

    @valid_attrs %{filename: "some filename",
		   size: 120.5,
		   file_data: %{data: "some data"}}
    @update_attrs %{filename: "some updated filename",
		    size: 456.7}
    @invalid_attrs %{data: nil, filename: nil, size: nil}

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

      %{course_id: course_id, uploader: user_id}
    end
    
    def file_fixture(attrs \\ %{}) do
      {:ok, file} =
        attrs
        |> Enum.into(ids_fixture())
        |> Enum.into(@valid_attrs)
        |> Files.create_file()

      Files.get_file!(file.id)
    end

    test "list_files/0 returns all files" do
      file = file_fixture()
      assert Files.list_files() == [file]
    end

    test "get_file!/1 returns the file with given id" do
      file = file_fixture()
      assert Files.get_file!(file.id) == file
    end

    test "create_file/1 with valid data creates a file" do
      attrs = ids_fixture()
      |> Enum.into(@valid_attrs)
      
      assert {:ok, %File{} = file} = Files.create_file(attrs)
      assert file.file_data.data == "some data"
      assert file.filename == "some filename"
      assert file.size == 120.5
    end

    test "create_file/1 with invalid data returns error changeset" do
      assert {:error, %Ecto.Changeset{}} = Files.create_file(@invalid_attrs)
    end

    test "update_file/2 with valid data updates the file" do
      file = file_fixture()
      assert {:ok, %File{} = file} = Files.update_file(file, @update_attrs)
      assert file.filename == "some updated filename"
      assert file.size == 456.7
    end

    test "update_file/2 with invalid data returns error changeset" do
      file = file_fixture()
      assert {:error, %Ecto.Changeset{}} = Files.update_file(file, @invalid_attrs)
      assert file == Files.get_file!(file.id)
    end

    test "delete_file/1 deletes the file" do
      file = file_fixture()
      assert {:ok, %File{}} = Files.delete_file(file)
      assert_raise Ecto.NoResultsError, fn -> Files.get_file!(file.id) end
    end

    test "change_file/1 returns a file changeset" do
      file = file_fixture()
      assert %Ecto.Changeset{} = Files.change_file(file)
    end
  end

  describe "file_data" do
    alias Schoolhub.Files.FileData

    @valid_attrs %{data: "some data"}
    @update_attrs %{data: "some updated data"}
    @invalid_attrs %{data: nil}

    def file_data_fixture(attrs \\ %{}) do
      {:ok, file_data} =
        attrs
        |> Enum.into(@valid_attrs)
        |> Files.create_file_data()

      file_data
    end

    test "list_file_data/0 returns all file_data" do
      file_data = file_data_fixture()
      assert Files.list_file_data() == [file_data]
    end

    test "get_file_data!/1 returns the file_data with given id" do
      file_data = file_data_fixture()
      assert Files.get_file_data!(file_data.id) == file_data
    end

    test "create_file_data/1 with valid data creates a file_data" do
      assert {:ok, %FileData{} = file_data} = Files.create_file_data(@valid_attrs)
      assert file_data.data == "some data"
    end

    test "create_file_data/1 with invalid data returns error changeset" do
      assert {:error, %Ecto.Changeset{}} = Files.create_file_data(@invalid_attrs)
    end

    test "update_file_data/2 with valid data updates the file_data" do
      file_data = file_data_fixture()
      assert {:ok, %FileData{} = file_data} = Files.update_file_data(file_data, @update_attrs)
      assert file_data.data == "some updated data"
    end

    test "update_file_data/2 with invalid data returns error changeset" do
      file_data = file_data_fixture()
      assert {:error, %Ecto.Changeset{}} = Files.update_file_data(file_data, @invalid_attrs)
      assert file_data == Files.get_file_data!(file_data.id)
    end

    test "delete_file_data/1 deletes the file_data" do
      file_data = file_data_fixture()
      assert {:ok, %FileData{}} = Files.delete_file_data(file_data)
      assert_raise Ecto.NoResultsError, fn -> Files.get_file_data!(file_data.id) end
    end

    test "change_file_data/1 returns a file_data changeset" do
      file_data = file_data_fixture()
      assert %Ecto.Changeset{} = Files.change_file_data(file_data)
    end
  end
end
