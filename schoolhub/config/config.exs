# This file is responsible for configuring your application
# and its dependencies with the aid of the Mix.Config module.
#
# This configuration file is loaded before any dependency and
# is restricted to this project.

# General application configuration
use Mix.Config

config :schoolhub,
  ecto_repos: [Schoolhub.Repo],
  auth_session_timeout: 1000

# Configures the endpoint
config :schoolhub, SchoolhubWeb.Endpoint,
  url: [host: "localhost"],
  secret_key_base: "W4TTUbIMqVWjs4mGVGkjgjAZdyBkWlpr/otkWLa9Q0/dwkVkiXArkj6+M01XosTZ",
  render_errors: [view: SchoolhubWeb.ErrorView, accepts: ~w(html json), layout: false],
  pubsub_server: Schoolhub.PubSub,
  live_view: [signing_salt: "8UjvKI2x"]

# Configures SMTP email server
config :schoolhub, Schoolhub.Mailer,
  adapter: Bamboo.SMTPAdapter,
  server: "localhost",
  port: 785,
  ssl: false,
  tls: :always,
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
