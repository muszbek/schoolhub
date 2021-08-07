# This file is responsible for configuring your application
# and its dependencies with the aid of the Mix.Config module.
#
# This configuration file is loaded before any dependency and
# is restricted to this project.

# General application configuration
use Mix.Config

config :schoolhub_router,
  ecto_repos: [SchoolhubRouter.Repo]

# Configures the endpoint
config :schoolhub_router, SchoolhubRouterWeb.Endpoint,
  url: [host: "localhost"],
  secret_key_base: "bFR2Q9vlIONUhb6IiGRIJ4TQ7x2L7afSMDR2qxHrnckaTiRwV8FGYHU5Yy2HHScI",
  render_errors: [view: SchoolhubRouterWeb.ErrorView, accepts: ~w(html json), layout: false],
  pubsub_server: SchoolhubRouter.PubSub,
  live_view: [signing_salt: "OQMmELra"]

# Configures SMTP email server
config :schoolhub_router, SchoolhubRouter.Mailer,
  adapter: Bamboo.SMTPAdapter,
  server: "localhost",
  port: 1587,
  ssl: false,
  tls: :if_available,
  retries: 1,
  no_mx_lookups: false

# Configures Elixir's Logger
config :logger, :console,
  format: "$time $metadata[$level] $message\n",
  metadata: [:request_id]

# Use Jason for JSON parsing in Phoenix
config :phoenix, :json_library, Jason

# Import environment specific config. This must remain at the bottom
# of this file so it overrides the configuration defined above.
import_config "#{Mix.env()}.exs"
