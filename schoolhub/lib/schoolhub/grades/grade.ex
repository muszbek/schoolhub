defmodule Schoolhub.Grades.Grade do
  use Ecto.Schema
  import Ecto.Changeset

  alias Schoolhub.Courses.Affiliation
  
  schema "grades" do
    field :grades, :map
    belongs_to :affiliation, Affiliation

    timestamps()
  end

  @doc false
  def changeset(grade, attrs) do
    grade
    |> cast(attrs, [:grades, :affiliation_id])
    |> validate_required([:grades])
    |> foreign_key_constraint(:affiliation_id)
  end
end
