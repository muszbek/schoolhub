defmodule Schoolhub.Courses.Affiliation do
  use Ecto.Schema
  import Ecto.Changeset

  alias Schoolhub.Courses.Course
  alias Schoolhub.Accounts.User
  
  schema "course_affiliations" do
    field :affiliation, :string,
      default: "student"
    belongs_to :course, Course
    belongs_to :user, User
    field :grades, :map

    timestamps()
  end

  @doc false
  def changeset(affiliation, attrs) do
    affiliation
    |> cast(attrs, [:affiliation, :course_id, :user_id, :grades])
    |> validate_required([:affiliation, :course_id, :user_id])
    |> validate_inclusion(:affiliation, ["student", "assistant", "owner"],
      message: "Affiliation level has to be one of the following: student, assistant, owner")
    |> foreign_key_constraint(:course_id)
    |> foreign_key_constraint(:user_id)
    |> unique_constraint([:course_id, :user_id])
  end
end
