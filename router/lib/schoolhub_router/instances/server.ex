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
    field :customer_id, :string
    field :last_paid, :utc_datetime
    field :last_unpaid, :utc_datetime

    timestamps()
  end

  @doc false
  def changeset(server, attrs) do
    server
    |> cast(attrs, [:name, :address, :admin_pw, :owner_email, :active,
		    :customer_id, :last_paid, :last_unpaid])
    |> validate_required([:name, :address, :owner_email])
    |> unique_constraint(:name)
    |> unique_constraint(:address)
  end

  def validate_changeset(server, attrs) do
    server
    |> cast(attrs, [:name, :admin_pw, :owner_email])
    |> validate_required([:name, :owner_email])
    |> unique_constraint(:name)
  end
end
