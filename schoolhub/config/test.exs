use Mix.Config

# Configure your database
#
# The MIX_TEST_PARTITION environment variable can be used
# to provide built-in test partitioning in CI environment.
# Run `mix help test` for more information.
config :schoolhub, Schoolhub.Repo,
  username: "schoolhub",
  password: "schoolhub",
  database: "schoolhub_test#{System.get_env("MIX_TEST_PARTITION")}",
  hostname: "10.3.0.3",
  ssl: true,
  ssl_opts: [verify: :verify_none],
  pool: Ecto.Adapters.SQL.Sandbox

# We don't run a server during test. If one is required,
# you can enable the server option below.
config :schoolhub, SchoolhubWeb.Endpoint,
  http: [port: 4002],
  server: false

# Print only warnings and errors during test
config :logger, level: :warn
