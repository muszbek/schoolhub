defmodule SchoolhubRouter.Repo.Migrations.UpdateServersOwnerEmail do
  use Ecto.Migration

  def change do
    alter table(:servers) do
      add :owner_email, :string
    end
  end
end
