defmodule SchoolhubRouter.Instances.Server do
  use Ecto.Schema
  import Ecto.Changeset

  schema "servers" do
    field :name, :string
    field :address, :string
    field :active, :boolean,
      default: true

    timestamps()
  end

  @doc false
  def changeset(server, attrs) do
    server
    |> cast(attrs, [:name, :address, :active])
    |> validate_required([:name, :address])
    |> unique_constraint(:name)
  end
end
