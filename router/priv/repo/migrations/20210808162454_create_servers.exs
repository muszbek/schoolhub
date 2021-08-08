defmodule SchoolhubRouter.Repo.Migrations.CreateServers do
  use Ecto.Migration

  def change do
    create table(:servers) do
      add :name, :string
      add :address, :string
      add :active, :boolean,
	default: true

      timestamps()
    end

    create unique_index(:servers, [:name])
    create unique_index(:servers, [:address])
  end
end
