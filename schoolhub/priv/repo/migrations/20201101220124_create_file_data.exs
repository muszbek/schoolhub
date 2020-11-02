defmodule Schoolhub.Repo.Migrations.CreateFileData do
  use Ecto.Migration

  def change do
    create table(:file_data) do
      add :data, :bytea
      add :file_id, references(:files, on_delete: :delete_all),
	null: false

      timestamps()
    end

    create index(:file_data, [:file_id])
  end
end
