defmodule SchoolhubRouter.Instances do
  @moduledoc """
  The Instances context.
  """

  import Ecto.Query, warn: false
  alias SchoolhubRouter.Repo

  alias SchoolhubRouter.Instances.Server
  alias SchoolhubRouter.Instances.K8sLib

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

  def create_server_with_k8s(attrs \\ %{}) do
    count = count_servers()
    ## server index starts with zero, so the count gives the index of the new one
    address = "schoolhub-instance-" <> to_string(count) <> ".schoolhub.default.svc.cluster.local"

    attrs_with_address = attrs
    |> Map.put(:address, address)
    |> Morphix.atomorphify!()

    case create_server(attrs_with_address) do
      {:ok, server} ->
	K8sLib.connect()
	|> K8sLib.scale(count+1)
	|> validate_server(server)
      {:error, error} ->  
	{:error, error}
    end
  end

  defp validate_server({:ok, scale}, _server), do: {:ok, scale}
  defp validate_server({:error, error}, server) do
    delete_server(server)
    {:error, error}
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
end
