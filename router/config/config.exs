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
  secret_key_base: System.get_env("SECRET_KEY_BASE",
    "/d8p4MwXOA3uA9jZ2fIouO5BwvwOIRM9Gqny3Cf7XFaY/pQ1Yukf53H9iy9AugqP"),
  render_errors: [view: SchoolhubRouterWeb.ErrorView, accepts: ~w(html json), layout: false],
  pubsub_server: SchoolhubRouter.PubSub,
  live_view: [signing_salt: "OQMmELra"]

config :schoolhub_router, SchoolhubRouter.Email,
  email_backend: System.get_env("EMAIL_USE_API", "")

# Configures SMTP email server
config :schoolhub_router, SchoolhubRouter.Mailer,
  adapter: Bamboo.SMTPAdapter,
  server: "localhost",
  port: 1587,
  ssl: false,
  tls: :if_available,
  retries: 1,
  no_mx_lookups: false

# Configures MailJet email API
config :schoolhub_router, SchoolhubRouter.Email.Http,
  mailjet_creds: System.get_env("MAILJET_CREDS", "dummy:dummy")

config :schoolhub_router, SchoolhubRouter.Instances,
  signing_salt: System.get_env("SERVER_SIGNING_SALT", "6MVU0xcD")

config :schoolhub_router, SchoolhubRouter.AdminLib,
  signing_salt: System.get_env("ADMIN_SIGNING_SALT" , "JIFUvxqk"),
  admin_password: System.get_env("ADMIN_PASSWORD", "dummy_password")

config :schoolhub_router, SchoolhubRouter.RecycleLib,
  http_impl: HTTPoison,
  phx_port: 1443,
  ssl_opts: [cacertfile: "priv/cert/chain.pem"]

# Configures Stripe
config :stripity_stripe,
  api_key: System.get_env("STRIPE_SECRET",
    "sk_test_51JfMahI0DC66QfKH5ZFJbyCRpehvusCSCwntEjlPLvuYBgEHcaEQ8GLuYFimTXnlAMSKxoH3bowXlpJa0dVrlX2z00qyKC7ie4"),
  webhook_secret: System.get_env("STRIPE_WH_SECRET", "")

# Configures Elixir's Logger
config :logger, :console,
  format: "$time $metadata[$level] $message\n",
  metadata: [:request_id]

# Use Jason for JSON parsing in Phoenix
config :phoenix, :json_library, Jason

# Import environment specific config. This must remain at the bottom
# of this file so it overrides the configuration defined above.
import_config "#{Mix.env()}.exs"
