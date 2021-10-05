defmodule SchoolhubRouter.Repo.Migrations.UpdateServersTableAdminPw do
  use Ecto.Migration

  def change do
    alter table(:servers) do
      add :admin_pw, :string
    end
  end
end
