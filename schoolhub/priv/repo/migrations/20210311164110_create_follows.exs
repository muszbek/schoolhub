defmodule Schoolhub.Repo.Migrations.CreateFollows do
  use Ecto.Migration

  def change do
    create table(:follows) do
      add :question_id, references(:questions, on_delete: :delete_all),
	null: false
      add :user_id, references(:user_profiles, on_delete: :delete_all),
	null: false

      timestamps()
    end

    create index(:follows, [:question_id])
    create index(:follows, [:user_id])
    create unique_index(:follows, [:question_id, :user_id])
  end
end
