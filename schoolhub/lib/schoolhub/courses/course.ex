defmodule Schoolhub.Courses.Course do
  use Ecto.Schema
  import Ecto.Changeset

  schema "courses" do
    field :description, :string
    field :name, :string
    field :active, :boolean,
      default: true

    timestamps()
  end

  @doc false
  def changeset(course, attrs) do
    course
    |> cast(attrs, [:name, :description, :active])
    |> validate_required([:name, :description])
  end
end
