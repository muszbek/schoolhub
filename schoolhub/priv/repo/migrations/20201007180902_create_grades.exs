defmodule Schoolhub.Repo.Migrations.CreateGrades do
  use Ecto.Migration

  def change do
    create table(:grades) do
      add :grades, :map
      add :affiliation_id, references(:course_affiliations, on_delete: :delete_all),
	null: false

      timestamps()
    end

    create index(:grades, [:affiliation_id])
  end
end
