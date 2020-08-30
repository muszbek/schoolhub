defmodule Schoolhub.AccountsTest do
  use Schoolhub.DataCase

  alias Schoolhub.Accounts

  describe "users" do
    alias Schoolhub.Accounts.User

    @valid_attrs %{email: "some email",
		   name: "some name",
		   credential: %{username: "some username",
				 password: "some password"}}
    @update_attrs %{email: "some updated email",
		    name: "some updated name",
		    credential: %{username: "some updated username",
				  password: "some updated password"}}
    @invalid_attrs %{email: nil, name: nil}

    def user_fixture(attrs \\ %{}) do
      {:ok, user} =
        attrs
        |> Enum.into(@valid_attrs)
        |> Accounts.create_user()

      user
    end

    test "list_users/0 returns all users" do
      user = user_fixture()
      assert Accounts.list_users() == [user]
    end

    test "get_user!/1 returns the user with given id" do
      user = user_fixture()
      assert Accounts.get_user!(user.id) == user
    end

    test "create_user/1 with valid data creates a user" do
      assert {:ok, %User{} = user} = Accounts.create_user(@valid_attrs)
      assert user.email == "some email"
      assert user.name == "some name"
    end

    test "create_user/1 with invalid data returns error changeset" do
      assert {:error, %Ecto.Changeset{}} = Accounts.create_user(@invalid_attrs)
    end

    test "update_user/2 with valid data updates the user" do
      user = user_fixture()
      assert {:ok, %User{} = user} = Accounts.update_user(user, @update_attrs)
      assert user.email == "some updated email"
      assert user.name == "some updated name"
    end

    test "update_user/2 with invalid data returns error changeset" do
      user = user_fixture()
      assert {:error, %Ecto.Changeset{}} = Accounts.update_user(user, @invalid_attrs)
      assert user == Accounts.get_user!(user.id)
    end

    test "delete_user/1 deletes the user" do
      user = user_fixture()
      assert {:ok, %User{}} = Accounts.delete_user(user)
      assert_raise Ecto.NoResultsError, fn -> Accounts.get_user!(user.id) end
    end

    test "change_user/1 returns a user changeset" do
      user = user_fixture()
      assert %Ecto.Changeset{} = Accounts.change_user(user)
    end
  end

  describe "credentials" do
    alias Schoolhub.Accounts.Credential

    @valid_user_attrs %{email: "some email",
			name: "some name",
			credential: %{username: "some other username",
				      password: "some other password"}}
    @valid_attrs %{pass_details: "some pass_details",
		   password: "some password",
		   username: "some username"}
    @update_attrs %{pass_details: "some updated pass_details",
		    password: "some updated password",
		    username: "some updated username"}
    @invalid_attrs %{pass_details: nil, password: nil, username: nil, user_id: nil}
    @scram_prefix "==SCRAM=="

    def credential_fixture(attrs \\ %{}) do
      {:ok, user} =
        attrs
        |> Enum.into(@valid_user_attrs)
        |> Accounts.create_user()

      %{credential: credential} = user
      credential
    end

    test "list_credentials/0 returns all credentials" do
      credential = credential_fixture()
      assert Accounts.list_credentials() == [credential]
    end

    test "get_credential!/1 returns the credential with given id" do
      credential = credential_fixture()
      assert Accounts.get_credential!(credential.username) == credential
    end

    test "create_credential/1 with valid data returns inserted credential" do
      some_credential = credential_fixture()
      %{user_id: user_id} = some_credential
      valid_attrs = Enum.into(%{user_id: user_id}, @valid_attrs)
      
      assert {:ok, %Credential{} = credential} = Accounts.create_credential(valid_attrs)
      assert credential.pass_details |> String.starts_with?(@scram_prefix)
      assert credential.password == ""
      assert credential.username == "some username"
    end

    test "create_credential/1 with invalid data returns error changeset" do
      assert {:error, %Ecto.Changeset{}} = Accounts.create_credential(@invalid_attrs)
    end

    test "update_credential/2 with valid data updates the credential" do
      credential = credential_fixture()
      assert {:ok, %Credential{} = credential} = Accounts.update_credential(credential, @update_attrs)
      assert credential.pass_details |> String.starts_with?(@scram_prefix)
      assert credential.password == ""
      assert credential.username == "some updated username"
    end

    test "update_credential/2 with invalid data returns error changeset" do
      credential = credential_fixture()
      assert {:error, %Ecto.Changeset{}} = Accounts.update_credential(credential, @invalid_attrs)
      assert credential == Accounts.get_credential!(credential.username)
    end

    test "delete_credential/1 deletes the credential" do
      credential = credential_fixture()
      assert {:ok, %Credential{}} = Accounts.delete_credential(credential)
      assert_raise Ecto.NoResultsError, fn -> Accounts.get_credential!(credential.username) end
    end

    test "change_credential/1 returns a credential changeset" do
      credential = credential_fixture()
      assert %Ecto.Changeset{} = Accounts.change_credential(credential)
    end
  end
end
