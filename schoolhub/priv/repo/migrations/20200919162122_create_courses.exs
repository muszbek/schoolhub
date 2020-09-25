defmodule Schoolhub.Repo.Migrations.CreateCourses do
  use Ecto.Migration

  def change do
    create table(:courses) do
      add :name, :string
      add :description, :text
      add :creator, references(:user_profiles)
      add :active, :boolean,
	default: true

      timestamps()
    end

    create index(:courses, [:creator])
  end
end
