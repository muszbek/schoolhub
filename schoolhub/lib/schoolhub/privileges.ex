defmodule Schoolhub.Privileges do
  @moduledoc """
  The Privileges context.
  """

  import Ecto.Query, warn: false
  alias Schoolhub.Repo

  alias Schoolhub.Privileges.Privilege

  @doc """
  Returns the list of privileges.

  ## Examples

      iex> list_privileges()
      [%Privilege{}, ...]

  """
  def list_privileges do
    Privilege
    |> order_by(desc: :level)
    |> Repo.all()
  end

  @doc """
  Gets a single privilege.

  Raises `Ecto.NoResultsError` if the Privilege does not exist.

  ## Examples

      iex> get_privilege!(123)
      %Privilege{}

      iex> get_privilege!(456)
      ** (Ecto.NoResultsError)

  """
  def get_privilege!(id), do: Repo.get!(Privilege, id)

  @doc """
  Creates a privilege.

  ## Examples

      iex> create_privilege(%{field: value})
      {:ok, %Privilege{}}

      iex> create_privilege(%{field: bad_value})
      {:error, %Ecto.Changeset{}}

  """
  def create_privilege(attrs \\ %{}) do
    %Privilege{}
    |> Privilege.changeset(attrs)
    |> Repo.insert()
  end

  @doc """
  Updates a privilege.

  ## Examples

      iex> update_privilege(privilege, %{field: new_value})
      {:ok, %Privilege{}}

      iex> update_privilege(privilege, %{field: bad_value})
      {:error, %Ecto.Changeset{}}

  """
  def update_privilege(%Privilege{} = privilege, attrs) do
    privilege
    |> Privilege.changeset(attrs)
    |> Repo.update()
  end

  @doc """
  Deletes a privilege.

  ## Examples

      iex> delete_privilege(privilege)
      {:ok, %Privilege{}}

      iex> delete_privilege(privilege)
      {:error, %Ecto.Changeset{}}

  """
  def delete_privilege(%Privilege{} = privilege) do
    Repo.delete(privilege)
  end

  @doc """
  Returns an `%Ecto.Changeset{}` for tracking privilege changes.

  ## Examples

      iex> change_privilege(privilege)
      %Ecto.Changeset{data: %Privilege{}}

  """
  def change_privilege(%Privilege{} = privilege, attrs \\ %{}) do
    Privilege.changeset(privilege, attrs)
  end
end
