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
  secret_key_base: System.get_env("SECRET_KEY_BASE",
    "/d8p4MwXOA3uA9jZ2fIouO5BwvwOIRM9Gqny3Cf7XFaY/pQ1Yukf53H9iy9AugqP"),
  render_errors: [view: SchoolhubWeb.ErrorView, accepts: ~w(html json), layout: false],
  pubsub_server: Schoolhub.PubSub,
  live_view: [signing_salt: System.get_env("LIVE_SIGNING_SALT", "8UjvKI2x")]

config :schoolhub, Schoolhub.Email,
  email_backend: System.get_env("EMAIL_USE_API", "")

# Configures SMTP email server
config :schoolhub, Schoolhub.Mailer,
  adapter: Bamboo.SMTPAdapter,
  server: "localhost",
  port: 1587,
  ssl: false,
  tls: :if_available,
  retries: 1,
  no_mx_lookups: false

# Configures MailJet email API
config :schoolhub, Schoolhub.Email.Http,
  mailjet_creds: System.get_env("MAILJET_CREDS", "dummy:dummy")

config :schoolhub, Schoolhub.Accounts,
  signing_salt: System.get_env("USER_SIGNING_SALT", "mKg5y9G6")

config :schoolhub, Schoolhub.AdminLib,
  http_adapter: HTTPoison,
  self_pod_name: System.get_env("POD_NAME", "schoolhub-instance-0"),
  router_host: "phx-server",
  router_port: 4001,
  ssl_opts: [cacertfile: "priv/cert/chain.pem"]

# Configures Elixir's Logger
config :logger, :console,
  format: "$time $metadata[$level] $message\n",
  metadata: [:request_id]

# Use Jason for JSON parsing in Phoenix
config :phoenix,
  json_library: Jason,
  template_engines: [leex: Phoenix.LiveView.Engine]

# Import environment specific config. This must remain at the bottom
# of this file so it overrides the configuration defined above.
import_config "#{Mix.env()}.exs"
