defmodule Schoolhub.Accounts.Credential do
  use Ecto.Schema
  import Ecto.Changeset

  alias Schoolhub.Accounts.User

  schema "users" do
    field :pass_details, :string
    field :password, :string
    field :username, :string
    belongs_to :user, User

    timestamps()
  end

  @doc false
  def changeset(credential, attrs) do
    credential
    |> cast(attrs, [:username, :password, :pass_details])
    |> validate_required([:username, :password])
    |> unique_constraint(:username)
  end
end
