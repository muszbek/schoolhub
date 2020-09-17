defmodule Schoolhub.PrivilegesTest do
  use Schoolhub.DataCase

  alias Schoolhub.Privileges

  describe "privileges" do
    alias Schoolhub.Privileges.Privilege

    @valid_attrs %{level: "student"}
    @update_attrs %{level: "teacher"}
    @invalid_attrs %{level: "some invalid level"}

    def privilege_fixture(attrs \\ %{}) do
      {:ok, privilege} =
        attrs
        |> Enum.into(@valid_attrs)
        |> Privileges.create_privilege()

      privilege
    end

    test "list_privileges/0 returns all privileges" do
      privilege = privilege_fixture()
      assert Privileges.list_privileges() == [privilege]
    end

    test "get_privilege!/1 returns the privilege with given id" do
      privilege = privilege_fixture()
      assert Privileges.get_privilege!(privilege.id) == privilege
    end

    test "create_privilege/1 with valid data creates a privilege" do
      assert {:ok, %Privilege{} = privilege} = Privileges.create_privilege(@valid_attrs)
      assert privilege.level == "some level"
    end

    test "create_privilege/1 with invalid data returns error changeset" do
      assert {:error, %Ecto.Changeset{}} = Privileges.create_privilege(@invalid_attrs)
    end

    test "update_privilege/2 with valid data updates the privilege" do
      privilege = privilege_fixture()
      assert {:ok, %Privilege{} = privilege} = Privileges.update_privilege(privilege, @update_attrs)
      assert privilege.level == "some updated level"
    end

    test "update_privilege/2 with invalid data returns error changeset" do
      privilege = privilege_fixture()
      assert {:error, %Ecto.Changeset{}} = Privileges.update_privilege(privilege, @invalid_attrs)
      assert privilege == Privileges.get_privilege!(privilege.id)
    end

    test "delete_privilege/1 deletes the privilege" do
      privilege = privilege_fixture()
      assert {:ok, %Privilege{}} = Privileges.delete_privilege(privilege)
      assert_raise Ecto.NoResultsError, fn -> Privileges.get_privilege!(privilege.id) end
    end

    test "change_privilege/1 returns a privilege changeset" do
      privilege = privilege_fixture()
      assert %Ecto.Changeset{} = Privileges.change_privilege(privilege)
    end
  end
end
