defmodule Schoolhub.Repo.Migrations.CreatePosts do
  use Ecto.Migration

  def change do
    create table(:posts) do
      add :content, :text
      add :pinned, :boolean, default: false, null: false
      add :course_id, references(:courses, on_delete: :delete_all),
	null: false
      add :creator, references(:user_profiles, on_delete: :nothing)

      timestamps()
    end

    create index(:posts, [:course_id])
    create index(:posts, [:creator])
  end
end
