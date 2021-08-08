defmodule SchoolhubRouter.InstancesTest do
  use SchoolhubRouter.DataCase

  alias SchoolhubRouter.Instances

  describe "servers" do
    alias SchoolhubRouter.Instances.Server

    @valid_attrs %{active: true, address: "some address", name: "some name"}
    @update_attrs %{active: false, address: "some updated address", name: "some updated name"}
    @invalid_attrs %{active: nil, address: nil, name: nil}

    def server_fixture(attrs \\ %{}) do
      {:ok, server} =
        attrs
        |> Enum.into(@valid_attrs)
        |> Instances.create_server()

      server
    end

    test "list_servers/0 returns all servers" do
      server = server_fixture()
      assert Instances.list_servers() == [server]
    end

    test "get_server!/1 returns the server with given id" do
      server = server_fixture()
      assert Instances.get_server!(server.id) == server
    end

    test "get_server_by_name/1 returns the server with given name" do
      server = server_fixture()
      assert Instances.get_server_by_name(server.name) == server
    end

    test "create_server/1 with valid data creates a server" do
      assert {:ok, %Server{} = server} = Instances.create_server(@valid_attrs)
      assert server.active == true
      assert server.address == "some address"
      assert server.name == "some name"
    end

    test "create_server/1 with invalid data returns error changeset" do
      assert {:error, %Ecto.Changeset{}} = Instances.create_server(@invalid_attrs)
    end

    test "update_server/2 with valid data updates the server" do
      server = server_fixture()
      assert {:ok, %Server{} = server} = Instances.update_server(server, @update_attrs)
      assert server.active == false
      assert server.address == "some updated address"
      assert server.name == "some updated name"
    end

    test "update_server/2 with invalid data returns error changeset" do
      server = server_fixture()
      assert {:error, %Ecto.Changeset{}} = Instances.update_server(server, @invalid_attrs)
      assert server == Instances.get_server!(server.id)
    end

    test "delete_server/1 deletes the server" do
      server = server_fixture()
      assert {:ok, %Server{}} = Instances.delete_server(server)
      assert_raise Ecto.NoResultsError, fn -> Instances.get_server!(server.id) end
    end

    test "change_server/1 returns a server changeset" do
      server = server_fixture()
      assert %Ecto.Changeset{} = Instances.change_server(server)
    end
  end
end
