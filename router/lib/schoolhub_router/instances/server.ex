defmodule SchoolhubRouter.Instances.Server do
  use Ecto.Schema
  import Ecto.Changeset

  schema "servers" do
    field :name, :string
    field :address, :string
    field :admin_pw, :string
    field :active, :boolean,
      default: true

    timestamps()
  end

  @doc false
  def changeset(server, attrs) do
    server
    |> cast(attrs, [:name, :address, :admin_pw, :active])
    |> validate_required([:name, :address])
    |> unique_constraint(:name)
    |> unique_constraint(:address)
  end
end
