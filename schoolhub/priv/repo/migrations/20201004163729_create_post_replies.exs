defmodule Schoolhub.Repo.Migrations.CreatePostReplies do
  use Ecto.Migration

  def change do
    create table(:post_replies) do
      add :content, :text
      add :creator, references(:user_profiles, on_delete: :nothing)
      add :parent_post, references(:posts, on_delete: :delete_all),
	null: false

      timestamps()
    end

    create index(:post_replies, [:creator])
    create index(:post_replies, [:parent_post])
  end
end
