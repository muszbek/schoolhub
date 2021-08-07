defmodule SchoolhubRouter.Repo do
  use Ecto.Repo,
    otp_app: :schoolhub_router,
    adapter: Ecto.Adapters.Postgres
end
