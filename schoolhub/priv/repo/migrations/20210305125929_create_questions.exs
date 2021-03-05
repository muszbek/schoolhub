defmodule Schoolhub.Repo.Migrations.CreateQuestions do
  use Ecto.Migration

  def change do
    create table(:questions) do
      add :content, :text
      add :tags, {:array, :string}, default: []
      add :pinned, :boolean, default: false, null: false
      add :course_id, references(:courses, on_delete: :delete_all),
	null: false
      add :creator, references(:user_profiles, on_delete: :nothing)

      timestamps()
    end

    create index(:questions, [:course_id])
    create index(:questions, [:creator])
  end
end
