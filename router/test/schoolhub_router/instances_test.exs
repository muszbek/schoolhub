defmodule SchoolhubRouter.InstancesTest do
  use SchoolhubRouter.DataCase

  alias SchoolhubRouter.Instances

  describe "servers" do
    alias SchoolhubRouter.Instances.Server

    @valid_attrs %{name: "some_name"}
    @valid_raw_attrs %{address: "some_address", name: "some_name",
		       admin_pw: "some_pw"}
    @update_attrs %{active: false, address: "some_updated_address", name: "some_updated_name",
		    admin_pw: nil}
    @invalid_attrs %{active: nil, address: nil, name: nil, admin_pw: nil}

    def server_fixture(attrs \\ %{}) do
      {:ok, server} =
        attrs
        |> Enum.into(@valid_raw_attrs)
        |> Instances.create_server()

      server
    end

    test "list_servers/0 returns all servers" do
      server = server_fixture()
      assert Instances.list_servers() == [server]
    end

    test "count_servers/0 returns one" do
      _server = server_fixture()
      assert Instances.count_servers() == 1
    end

    test "get_server!/1 returns the server with given id" do
      server = server_fixture()
      assert Instances.get_server!(server.id) == server
    end

    test "get_server_by_name/1 returns the server with given name" do
      server = server_fixture()
      assert Instances.get_server_by_name(server.name) == server
    end
    
    test "get_server_by_name/1 returns nothing" do
      assert Instances.get_server_by_name("non_existing_name") == nil
    end

    test "get_server_by_address/1 returns the server with given address" do
      server = server_fixture()
      assert Instances.get_server_by_address(server.address) == server
    end
    
    test "get_server_by_address/1 returns nothing" do
      assert Instances.get_server_by_address("non_existing_address") == nil
    end

    test "create_server/1 with valid data creates a server" do
      assert {:ok, %Server{} = server} = Instances.create_server(@valid_raw_attrs)
      assert server.active == true
      assert server.address == "some_address"
      assert server.name == "some_name"
      assert server.admin_pw == "some_pw"
    end

    test "create_server/1 with invalid data returns error changeset" do
      assert {:error, %Ecto.Changeset{}} = Instances.create_server(@invalid_attrs)
    end

    test "create_server_with_k8s/1 with valid data returns error cannot connect to k8s" do
      assert {:error, :enoent} = Instances.create_server_with_k8s(@valid_attrs)
    end

    test "synchronize_with_k8s/0 returns error cannot connect to k8s" do
      _server = server_fixture()
      assert {:error, :enoent} = Instances.synchronize_with_k8s()
    end

    test "update_server/2 with valid data updates the server" do
      server = server_fixture()
      assert {:ok, %Server{} = server} = Instances.update_server(server, @update_attrs)
      assert server.active == false
      assert server.address == "some_updated_address"
      assert server.name == "some_updated_name"
      assert server.admin_pw == nil
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
