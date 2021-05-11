defmodule Schoolhub.Repo.Migrations.UpdateCoursesTable do
  use Ecto.Migration

  def change do
    alter table(:courses) do
      add :picture, :bytea
    end
  end
end
