defmodule SchoolhubRouter.Repo.Migrations.UpdateServersTableStripe do
  use Ecto.Migration

  def change do
    alter table(:servers) do
      add :customer_id, :string
      add :last_paid, :utc_datetime
      add :last_unpaid, :utc_datetime
    end
  end
end
