defmodule Schoolhub.Repo.Migrations.CreateUsers do
  use Ecto.Migration

  def change do
    create_if_not_exists table(:users, primary_key: false) do
      add :username, :string,
	primary_key: true
      add :password, :text
      add :pass_details, :text
      add :created_at, :naive_datetime_usec,
	default: fragment("now()"),
	null: false
    end
    
    alter table(:users) do
      add :user_id, references(:user_profiles, on_delete: :delete_all),
	null: false
      remove :created_at, :naive_datetime_usec,
	default: fragment("now()"),
	null: false
      
      timestamps()
    end
    
    create index(:users, [:user_id])
  end
end
