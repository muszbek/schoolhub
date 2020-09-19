defmodule Schoolhub.Repo.Migrations.CreateCourses do
  use Ecto.Migration

  def change do
    create table(:courses) do
      add :name, :string
      add :description, :text
      add :active, :boolean,
	default: true

      timestamps()
    end

  end
end
