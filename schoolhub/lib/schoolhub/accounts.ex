defmodule Schoolhub.Accounts do
  @moduledoc """
  The Accounts context.
  """
  
  import Ecto.Query, warn: false
  alias Schoolhub.Repo
  alias Phoenix.Token

  alias Schoolhub.Accounts.{User, Credential}
  alias Schoolhub.Privileges.Privilege

  @default_privilege "student"

  @doc """
  Returns the list of users.

  ## Examples

      iex> list_users()
      [%User{}, ...]

  """
  def list_users do
    User
    |> Repo.all()
    |> Repo.preload(:credential)
    |> Repo.preload(:privilege)
  end

  @doc """
  Gets a single user.

  Raises `Ecto.NoResultsError` if the User does not exist.

  ## Examples

      iex> get_user!(123)
      %User{}

      iex> get_user!(456)
      ** (Ecto.NoResultsError)

  """
  def get_user!(id) do
    User
    |> Repo.get!(id)
    |> Repo.preload(:credential)
    |> Repo.preload(:privilege)
  end

  @doc """
  Creates a user.

  ## Examples

      iex> create_user(%{field: value})
      {:ok, %User{}}

      iex> create_user(%{field: bad_value})
      {:error, %Ecto.Changeset{}}

  """
  def create_user(attrs \\ %{}) do
    attrs_with_privilege = attrs
    |> Map.put("privilege", %{level: @default_privilege})
    |> Morphix.atomorphify!()
    
    %User{}
    |> User.changeset(attrs_with_privilege)
    |> Ecto.Changeset.cast_assoc(:credential, with: &Credential.changeset/2)
    |> Ecto.Changeset.cast_assoc(:privilege, with: &Privilege.changeset/2)
    |> Repo.insert()
  end

  @doc """
  Updates a user.

  ## Examples

      iex> update_user(user, %{field: new_value})
      {:ok, %User{}}

      iex> update_user(user, %{field: bad_value})
      {:error, %Ecto.Changeset{}}

  """
  def update_user(%User{} = user, attrs) do
    user
    |> User.changeset(attrs)
    |> Ecto.Changeset.cast_assoc(:credential, with: &Credential.changeset/2)
    |> Ecto.Changeset.cast_assoc(:privilege, with: &Privilege.changeset/2)
    |> Repo.update()
  end

  @doc """
  Deletes a user.

  ## Examples

      iex> delete_user(user)
      {:ok, %User{}}

      iex> delete_user(user)
      {:error, %Ecto.Changeset{}}

  """
  def delete_user(%User{} = user) do
    Repo.delete(user)
  end

  @doc """
  Returns an `%Ecto.Changeset{}` for tracking user changes.

  ## Examples

      iex> change_user(user)
      %Ecto.Changeset{data: %User{}}

  """
  def change_user(%User{} = user, attrs \\ %{}) do
    User.changeset(user, attrs)
  end

  @doc """
  Returns the list of credentials.

  ## Examples

      iex> list_credentials()
      [%Credential{}, ...]

  """
  def list_credentials do
    Repo.all(Credential)
  end

  @doc """
  Gets a single credential.

  Raises `Ecto.NoResultsError` if the Credential does not exist.

  ## Examples

      iex> get_credential!(123)
      %Credential{}

      iex> get_credential!(456)
      ** (Ecto.NoResultsError)

  """
  def get_credential!(id) do
    username = to_string(id)
    Repo.get!(Credential, username)
  end

  def get_user_by_name!(username) do
    credential = Repo.get!(Credential, username)
    get_user!(credential.user_id)
  end

  def get_user_by_email(email) do
    User
    |> where(email: ^email)
    |> Repo.one()
    |> Repo.preload(:credential)
    |> Repo.preload(:privilege)
  end

  @doc """
  Creates a credential.

  ## Examples

      iex> create_credential(%{field: value})
      {:ok, %Credential{}}

      iex> create_credential(%{field: bad_value})
      {:error, %Ecto.Changeset{}}

  """
  def create_credential(attrs \\ %{}) do
    %Credential{}
    |> Credential.changeset(attrs)
    |> Repo.insert()
  end

  @doc """
  Updates a credential.

  ## Examples

      iex> update_credential(credential, %{field: new_value})
      {:ok, %Credential{}}

      iex> update_credential(credential, %{field: bad_value})
      {:error, %Ecto.Changeset{}}

  """
  def update_credential(%Credential{} = credential, attrs) do
    credential
    |> Credential.changeset(attrs)
    |> Repo.update()
  end

  @doc """
  Deletes a credential.

  ## Examples

      iex> delete_credential(credential)
      {:ok, %Credential{}}

      iex> delete_credential(credential)
      {:error, %Ecto.Changeset{}}

  """
  def delete_credential(%Credential{} = credential) do
    Repo.delete(credential)
  end

  @doc """
  Returns an `%Ecto.Changeset{}` for tracking credential changes.

  ## Examples

      iex> change_credential(credential)
      %Ecto.Changeset{data: %Credential{}}

  """
  def change_credential(%Credential{} = credential, attrs \\ %{}) do
    Credential.changeset(credential, attrs)
  end


  def create_token(username) do
    salt = Application.get_env(:schoolhub, __MODULE__)[:signing_salt]
    _token = Token.sign(SchoolhubWeb.Endpoint, salt, username)
  end

  def verify_token(token, max_age \\ 86400) do
    salt = Application.get_env(:schoolhub, __MODULE__)[:signing_salt]
    Token.verify(SchoolhubWeb.Endpoint, salt, token, max_age: max_age)
  end


  def authenticate("authenticated", username) do
    query =
      from u in User,
        inner_join: c in assoc(u, :credential),
        where: c.username == ^username

    case Repo.one(query) do
      %User{} = user -> {:ok, user}
      nil -> {:error, :unauthorized}
    end
  end
  def authenticate(_error, _username) do
    {:error, :unauthorized}
  end
  
end
