defmodule Schoolhub.Repo.Migrations.CreateQuestionReplies do
  use Ecto.Migration

  def change do
    create table(:question_replies) do
      add :content, :text
      add :creator, references(:user_profiles, on_delete: :nothing)
      add :parent_question, references(:questions, on_delete: :delete_all),
	null: false

      timestamps()
    end

    create index(:question_replies, [:creator])
    create index(:question_replies, [:parent_question])
  end
end
