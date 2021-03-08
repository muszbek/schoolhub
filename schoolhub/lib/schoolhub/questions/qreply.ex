defmodule Schoolhub.Questions.Qreply do
  use Ecto.Schema
  import Ecto.Changeset

  alias Schoolhub.Accounts.User
  alias Schoolhub.Questions.Question
 
  schema "question_replies" do
    field :content, :string
    belongs_to :question, Question,
      foreign_key: :parent_question
    belongs_to :user, User,
      foreign_key: :creator

    timestamps()
  end

  @doc false
  def changeset(qreply, attrs) do
    qreply
    |> cast(attrs, [:content, :parent_question, :creator])
    |> validate_required([:content, :parent_question, :creator])
    |> foreign_key_constraint(:parent_question)
    |> foreign_key_constraint(:creator)
  end
end
