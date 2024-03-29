defmodule Schoolhub.Courses.Course do
  use Ecto.Schema
  import Ecto.Changeset

  alias Schoolhub.Accounts.User
  alias Schoolhub.Courses.Affiliation
  alias Schoolhub.Posts.Post
  alias Schoolhub.Questions.Question

  schema "courses" do
    field :description, :string
    field :name, :string
    field :picture, :binary
    belongs_to :user, User,
      foreign_key: :creator
    field :active, :boolean,
      default: true
    has_many :affiliation, Affiliation
    has_many :post, Post
    has_many :question, Question

    timestamps()
  end

  @doc false
  def changeset(course, attrs) do
    course
    |> cast(attrs, [:name, :description, :picture, :active, :creator])
    |> validate_required([:name, :description])
    |> foreign_key_constraint(:creator)
    |> unique_constraint(:name)
  end
end
