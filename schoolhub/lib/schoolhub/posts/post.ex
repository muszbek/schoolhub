defmodule Schoolhub.Posts.Post do
  use Ecto.Schema
  import Ecto.Changeset

  alias Schoolhub.Accounts.User
  alias Schoolhub.Courses.Course

  schema "posts" do
    field :content, :string
    belongs_to :course, Course
    belongs_to :user, User,
      foreign_key: :creator
    field :pinned, :boolean, default: false

    timestamps()
  end

  @doc false
  def changeset(post, attrs) do
    post
    |> cast(attrs, [:content, :course_id, :creator, :pinned])
    |> validate_required([:content, :course_id, :creator, :pinned])
    |> foreign_key_constraint(:course_id)
    |> foreign_key_constraint(:creator)
  end
end
