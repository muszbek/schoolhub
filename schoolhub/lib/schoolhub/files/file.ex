defmodule Schoolhub.Files.File do
  use Ecto.Schema
  import Ecto.Changeset

  alias Schoolhub.Courses.Course
  alias Schoolhub.Accounts.User
  
  schema "files" do
    field :data, :binary
    field :filename, :string
    field :size, :float
    belongs_to :course, Course
    belongs_to :user, User,
      foreign_key: :uploader

    timestamps()
  end

  @doc false
  def changeset(file, attrs) do
    file
    |> cast(attrs, [:filename, :data, :size, :course_id, :uploader])
    |> validate_required([:filename, :data, :size, :course_id])
    |> unique_constraint(:filename)
    |> foreign_key_constraint(:course_id)
    |> foreign_key_constraint(:uploader)
  end
end
