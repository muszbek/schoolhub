defmodule Schoolhub.Repo do
  use Ecto.Repo,
    otp_app: :schoolhub,
    adapter: Ecto.Adapters.Postgres
end
