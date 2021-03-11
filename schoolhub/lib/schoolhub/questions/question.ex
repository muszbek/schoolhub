defmodule Schoolhub.Questions.Question do
  use Ecto.Schema
  import Ecto.Changeset

  alias Schoolhub.Accounts.User
  alias Schoolhub.Courses.Course
  alias Schoolhub.Questions.{Qreply, Follow}

  schema "questions" do
    field :content, :string
    belongs_to :course, Course
    belongs_to :user, User,
      foreign_key: :creator
    field :tags, {:array, :string}, default: []
    field :pinned, :boolean, default: false
    has_many :qreply, Qreply,
      foreign_key: :parent_question
    has_many :follow, Follow

    timestamps()
  end

  @doc false
  def changeset(question, attrs) do
    question
    |> cast(attrs, [:content, :course_id, :creator, :tags, :pinned])
    |> validate_required([:content, :course_id, :creator, :tags, :pinned])
    |> foreign_key_constraint(:course_id)
    |> foreign_key_constraint(:creator)
  end
end
