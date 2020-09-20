defmodule Schoolhub.Privileges.Privilege do
  use Ecto.Schema
  import Ecto.Changeset

  alias Schoolhub.Accounts.User

  schema "privileges" do
    field :level, :string,
      default: "student"
    belongs_to :user, User

    timestamps()
  end

  @doc false
  def changeset(privilege, attrs) do
    privilege
    |> cast(attrs, [:level, :user_id])
    |> validate_required([:level])
    |> validate_inclusion(:level, ["student", "teacher", "admin"],
      message: "Privilege level has to be one of the following: student, teacher, admin")
    |> foreign_key_constraint(:user_id)
  end
end
