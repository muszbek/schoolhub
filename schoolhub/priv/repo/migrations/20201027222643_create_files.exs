defmodule Schoolhub.Repo.Migrations.CreateFiles do
  use Ecto.Migration

  def change do
    create table(:files) do
      add :filename, :string
      add :size, :float
      add :course_id, references(:courses, on_delete: :nothing)
      add :uploader, references(:user_profiles, on_delete: :nothing)

      timestamps()
    end

    create unique_index(:files, [:filename])
    create index(:files, [:course_id])
    create index(:files, [:uploader])
  end
end
