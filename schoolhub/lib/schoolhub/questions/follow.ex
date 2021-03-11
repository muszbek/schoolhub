defmodule Schoolhub.Questions.Follow do
  use Ecto.Schema
  import Ecto.Changeset

  alias Schoolhub.Questions.Question
  alias Schoolhub.Accounts.User
  
  schema "follows" do
    belongs_to :question, Question
    belongs_to :user, User

    timestamps()
  end

  @doc false
  def changeset(follow, attrs) do
    follow
    |> cast(attrs, [:question_id, :user_id])
    |> validate_required([:question_id, :user_id])
    |> foreign_key_constraint(:question_id)
    |> foreign_key_constraint(:user_id)
    |> unique_constraint([:question_id, :user_id])
  end
end
