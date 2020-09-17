defmodule Schoolhub.Accounts.User do
  use Ecto.Schema
  import Ecto.Changeset

  alias Schoolhub.Accounts.Credential
  alias Schoolhub.Privileges.Privilege

  schema "user_profiles" do
    field :email, :string
    field :name, :string
    has_one :credential, Credential, on_replace: :update
    has_one :privilege, Privilege, on_replace: :update

    timestamps()
  end

  @doc false
  def changeset(user, attrs) do
    user
    |> cast(attrs, [:name, :email])
    |> validate_required([:name, :email])
    |> unique_constraint(:email)
  end
end
