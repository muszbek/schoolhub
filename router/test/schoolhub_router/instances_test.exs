defmodule SchoolhubRouter.InstancesTest do
  use SchoolhubRouter.DataCase

  alias SchoolhubRouter.Instances

  describe "servers" do
    alias SchoolhubRouter.Instances.Server

    @valid_attrs %{name: "some_name", admin_pw: "some_pw", owner_email: "some_email"}
    @valid_raw_attrs %{address: "some_address", name: "some_name",
		       admin_pw: "some_pw", owner_email: "some_email"}
    @valid_other_raw_attrs %{address: "other_address", name: "other_name",
			     admin_pw: "other_pw", owner_email: "other_email"}
    @update_attrs %{active: false, address: "some_updated_address", name: "some_updated_name",
		    owner_email: "some_updated_email", admin_pw: nil}
    @invalid_attrs %{active: nil, address: nil, name: nil, owner_email: nil, admin_pw: nil}

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

    test "get_server_by_customer/1 returns the server with given customer_id" do
      {:ok, server} = server_fixture()
      |> Instances.update_server(%{customer_id: "some_customer_id"})
      assert Instances.get_server_by_customer(server.customer_id) == server
    end

    test "get_server_by_customer/1 returns nothing" do
      assert Instances.get_server_by_customer("non_existing_customer_id") == nil
    end

    test "get_inactive_server/0 returns nothing" do
      _server = server_fixture()
      assert Instances.get_inactive_server() == nil
    end

    test "get_inactive_server/0 returns server" do
      server = server_fixture()
      {:ok, inactive_server} = Instances.update_server(server, %{active: false})
      assert Instances.get_inactive_server() == inactive_server
    end

    test "create_server/1 with valid data creates a server" do
      assert {:ok, %Server{} = server} = Instances.create_server(@valid_raw_attrs)
      assert server.active == true
      assert server.address == "some_address"
      assert server.name == "some_name"
      assert server.owner_email == "some_email"
      assert server.admin_pw == "some_pw"
    end

    test "create_server/1 with invalid data returns error changeset" do
      assert {:error, %Ecto.Changeset{}} = Instances.create_server(@invalid_attrs)
    end

    test "validate_server/1 with valid data returns ok" do
      assert {:ok, @valid_attrs} = Instances.validate_server(@valid_attrs)
    end

    test "validate_server/1 with invalid data returns error and changeset" do
      assert {:error, %Ecto.Changeset{}} = Instances.validate_server(@invalid_attrs)
    end

    test "recycle_server/2 with valid data recycles a server" do
      some_server = server_fixture()
      {:ok, inactive_server} = Instances.update_server(some_server, %{active: false})
      assert {:ok, %Server{} = server} =
	Instances.recycle_server(inactive_server, @valid_other_raw_attrs)
      assert server.active == true
      assert server.address == "other_address"
      assert server.name == "other_name"
      assert server.owner_email == "other_email"
      assert server.admin_pw == "other_pw"
    end

    test "recycle_server/2 with invalid data returns error changeset" do
      some_server = server_fixture()
      {:ok, inactive_server} = Instances.update_server(some_server, %{active: false})
      assert {:error, %Ecto.Changeset{}} =
	Instances.recycle_server(inactive_server, @invalid_attrs)
    end

    test "commission_server/1 with valid data returns ok" do
      assert :ok = Instances.commission_server(@valid_attrs)
    end

    test "commission_server/1 with invalid data returns error changeset" do
      assert {:error, %Ecto.Changeset{}} = Instances.commission_server(@invalid_attrs)
    end

    test "create_server_with_k8s/1 with valid data returns server" do
      assert {:ok, %Server{}} = Instances.create_server_with_k8s(@valid_attrs)
    end

    test "create_server_with_k8s/1 with invalid data returns error changeset" do
      assert {:error, %Ecto.Changeset{}} = Instances.create_server_with_k8s(@invalid_attrs)
    end

    test "synchronize_with_k8s/0 returns scale" do
      _server = server_fixture()
      assert Instances.synchronize_with_k8s() == {:ok, 1}
    end

    test "update_server/2 with valid data updates the server" do
      server = server_fixture()
      assert {:ok, %Server{} = server} = Instances.update_server(server, @update_attrs)
      assert server.active == false
      assert server.address == "some_updated_address"
      assert server.name == "some_updated_name"
      assert server.owner_email == "some_updated_email"
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
