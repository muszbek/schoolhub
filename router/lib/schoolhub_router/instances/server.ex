defmodule SchoolhubRouter.Instances.Server do
  use Ecto.Schema
  import Ecto.Changeset

  schema "servers" do
    field :name, :string
    field :address, :string
    field :admin_pw, :string
    field :owner_email, :string
    field :active, :boolean,
      default: true

    timestamps()
  end

  @doc false
  def changeset(server, attrs) do
    server
    |> cast(attrs, [:name, :address, :admin_pw, :owner_email, :active])
    |> validate_required([:name, :address, :owner_email])
    |> unique_constraint(:name)
    |> unique_constraint(:address)
  end
end
