defmodule SchoolhubRouter.Instances do
  @moduledoc """
  The Instances context.
  """

  import Ecto.Query, warn: false
  alias SchoolhubRouter.Repo
  
  alias Phoenix.Token
  alias SchoolhubRouter.Instances.Server
  alias SchoolhubRouter.RecycleLib
  alias SchoolhubRouter.Email

  @pod_address_suffix ".schoolhub.default.svc.cluster.local"
  
  @doc """
  Returns the list of servers.

  ## Examples

      iex> list_servers()
      [%Server{}, ...]

  """
  def list_servers do
    Repo.all(Server)
  end

  def count_servers do
    servers = list_servers()
    Enum.count(servers)
  end

  @doc """
  Gets a single server.

  Raises `Ecto.NoResultsError` if the Server does not exist.

  ## Examples

      iex> get_server!(123)
      %Server{}

      iex> get_server!(456)
      ** (Ecto.NoResultsError)

  """
  def get_server!(id), do: Repo.get!(Server, id)

  def get_server_by_name(name) do
    Server
    |> where(name: ^name)
    |> Repo.one()
  end

  def get_server_by_address(address) do
    Server
    |> where(address: ^address)
    |> Repo.one()
  end

  def get_server_by_customer(customer_id) do
    Server
    |> where(customer_id: ^customer_id)
    |> Repo.one()
  end

  @doc """
  Creates a server.

  ## Examples

      iex> create_server(%{field: value})
      {:ok, %Server{}}

      iex> create_server(%{field: bad_value})
      {:error, %Ecto.Changeset{}}

  """
  def create_server(attrs \\ %{}) do
    %Server{}
    |> Server.changeset(attrs)
    |> Repo.insert()
  end

  def validate_server(attrs \\ %{}) do
    changeset = %Server{}
    |> Server.validate_changeset(attrs)

    if changeset.valid? do
      {:ok, attrs}
    else
      {:error, changeset}
    end
  end

  def commission_server(attrs \\ %{}) do
    case do_commission_server(attrs) do
      {:ok, server} ->
	Email.confirm_reg_email(server)
	:ok
      {:error, error} ->
	{:error, error}
    end
  end
  
  defp do_commission_server(attrs) do
    case get_inactive_server() do
      nil -> create_server_with_k8s(attrs)
      server -> recycle_server(server, attrs)
    end
  end
  
  def get_inactive_server() do
    case get_inactive_servers() do
      [] -> nil
      [first | _rest] -> first
    end
  end
  
  def get_inactive_servers() do
    Server
    |> where(active: false)
    |> Repo.all()
  end

  def recycle_server(%Server{} = server, attrs \\ %{}) do
    attrs_with_active = attrs
    |> Map.put(:active, true)
    |> Morphix.atomorphify!()

    update_server(server, attrs_with_active)
    |> insert_admin()
  end

  defp insert_admin({:ok, server}) do
    spawn(fn -> RecycleLib.insert_admin(server) end)
    {:ok, server}
  end
  defp insert_admin({:error, error}), do: {:error, error}


  def create_server_with_k8s(attrs \\ %{}) do
    count = count_servers()
    ## server index starts with zero, so the count gives the index of the new one
    address = "schoolhub-instance-" <> to_string(count) <> @pod_address_suffix

    attrs_with_address = attrs
    |> Map.put(:address, address)
    |> Morphix.atomorphify!()

    create_server(attrs_with_address)
    |> do_create_server_with_k8s(count)
  end

  defp do_create_server_with_k8s({:ok, server}, count) do
    case k8s_scale(server, count) do
      {:ok, %{"kind" => "Scale"}} -> {:ok, server}
      {:error, error} -> {:error, error}
    end
  end
  defp do_create_server_with_k8s({:error, error}, _count), do: {:error, error}

  defp k8s_scale(server, count) do
    k8s_impl = k8s_impl()
    
    k8s_impl.connect()
    |> k8s_impl.scale(count+1)
    |> validate_server(server)
  end

  defp validate_server({:ok, scale}, _server), do: {:ok, scale}
  defp validate_server({:error, error}, server) do
    delete_server(server)
    {:error, error}
  end

  def synchronize_with_k8s() do
    count = count_servers()
    k8s_impl = k8s_impl()
    
    {:ok, %{"kind" => "Scale"}} = k8s_impl.connect()
    |> k8s_impl.scale(count)

    {:ok, count}
  end

  @doc """
  Updates a server.

  ## Examples

      iex> update_server(server, %{field: new_value})
      {:ok, %Server{}}

      iex> update_server(server, %{field: bad_value})
      {:error, %Ecto.Changeset{}}

  """
  def update_server(%Server{} = server, attrs) do
    server
    |> Server.changeset(attrs)
    |> Repo.update()
  end

  @doc """
  Deletes a server.

  ## Examples

      iex> delete_server(server)
      {:ok, %Server{}}

      iex> delete_server(server)
      {:error, %Ecto.Changeset{}}

  """
  def delete_server(%Server{} = server) do
    Repo.delete(server)
  end

  @doc """
  Returns an `%Ecto.Changeset{}` for tracking server changes.

  ## Examples

      iex> change_server(server)
      %Ecto.Changeset{data: %Server{}}

  """
  def change_server(%Server{} = server, attrs \\ %{}) do
    Server.changeset(server, attrs)
  end

  
  def create_token(username) do
    salt = Application.get_env(:schoolhub_router, __MODULE__)[:signing_salt]
    _token = Token.sign(SchoolhubRouterWeb.Endpoint, salt, username)
  end

  def verify_token(token, max_age \\ 86400) do
    salt = Application.get_env(:schoolhub_router, __MODULE__)[:signing_salt]
    Token.verify(SchoolhubRouterWeb.Endpoint, salt, token, max_age: max_age)
  end


  defp k8s_impl() do
    Application.get_env(:schoolhub_router, __MODULE__)[:k8s_impl]
  end
  
end
