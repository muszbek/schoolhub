defmodule Schoolhub.Accounts.Credential do
  use Ecto.Schema
  import Ecto.Changeset

  alias Schoolhub.Accounts.User
  alias Schoolhub.Accounts.ScramLib, as: CredLib

  @primary_key {:username, :string, []}
  @derive {Phoenix.Param, key: :username}
  schema "users" do
    field :pass_details, :string
    field :password, :string
    belongs_to :user, User

    timestamps()
  end

  @doc false
  def changeset(credential, attrs) do
    credential
    |> cast(attrs, [:username, :password, :pass_details, :user_id])
    |> validate_required([:username, :password])
    |> unique_constraint(:username)
    |> foreign_key_constraint(:user_id)
    |> CredLib.encode_password_in_changeset()
  end
end
