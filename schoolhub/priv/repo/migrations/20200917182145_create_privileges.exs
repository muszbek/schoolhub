defmodule Schoolhub.Repo.Migrations.CreatePrivileges do
  use Ecto.Migration

  def change do
    execute("CREATE TYPE privilege AS ENUM ('student', 'teacher', 'admin')")
    
    create table(:privileges) do
      add :level, :privilege,
	default: "student"
      add :user_id, references(:user_profiles, on_delete: :delete_all),
	null: false

      timestamps()
    end

    create index(:privileges, [:user_id])
  end
end
