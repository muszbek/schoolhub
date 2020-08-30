defmodule Schoolhub.Repo.Migrations.CreateUserProfiles do
  use Ecto.Migration

  def change do
    create table(:user_profiles) do
      add :name, :string
      add :email, :string

      timestamps()
    end

    create unique_index(:user_profiles, [:email])
  end
end
