defmodule Schoolhub.Repo.Migrations.CreateCourseAffiliations do
  use Ecto.Migration

  def change do
    create table(:course_affiliations) do
      add :affiliation, :string,
	default: "student"
      add :course_id, references(:courses, on_delete: :delete_all),
	null: false
      add :user_id, references(:user_profiles, on_delete: :delete_all),
	null: false
      add :grades, :map

      timestamps()
    end

    create index(:course_affiliations, [:course_id])
    create index(:course_affiliations, [:user_id])
    create unique_index(:course_affiliations, [:course_id, :user_id])
  end
end
