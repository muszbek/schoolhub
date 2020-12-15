defmodule Schoolhub.Repo.Migrations.CreateCourseAffiliations do
  use Ecto.Migration

  def change do
    execute("CREATE TYPE affiliation AS ENUM ('student', 'assistant', 'owner')")
    
    create table(:course_affiliations) do
      add :affiliation, :affiliation,
	default: "student"
      add :course_id, references(:courses, on_delete: :delete_all),
	null: false
      add :user_id, references(:user_profiles, on_delete: :delete_all),
	null: false

      timestamps()
    end

    create index(:course_affiliations, [:course_id])
    create index(:course_affiliations, [:user_id])
    create unique_index(:course_affiliations, [:course_id, :user_id])
  end
end
