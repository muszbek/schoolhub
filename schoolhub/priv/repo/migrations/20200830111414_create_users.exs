defmodule Schoolhub.Repo.Migrations.CreateUsers do
  use Ecto.Migration

  def change do
    create table(:users) do
      add :username, :string,
	null: false
      add :password, :text
      add :pass_details, :text
      add :user_id, references(:user_profiles, on_delete: :delete_all),
	null: false

      timestamps()
    end

    create unique_index(:users, [:username])
    create index(:users, [:user_id])
  end
end
