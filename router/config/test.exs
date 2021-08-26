use Mix.Config

# Configure your database
#
# The MIX_TEST_PARTITION environment variable can be used
# to provide built-in test partitioning in CI environment.
# Run `mix help test` for more information.
config :schoolhub_router, SchoolhubRouter.Repo,
  username: "schoolhub_router",
  password: "schoolhub_router",
  database: "schoolhub_router_test#{System.get_env("MIX_TEST_PARTITION")}",
  hostname: "localhost",
  pool: Ecto.Adapters.SQL.Sandbox

# We don't run a server during test. If one is required,
# you can enable the server option below.
config :schoolhub_router, SchoolhubRouterWeb.Endpoint,
  http: [port: 4002],
  server: false

config :schoolhub_router, SchoolhubRouter.Mailer,
  adapter: BambooSMTP.TestAdapter

config :schoolhub_router, SchoolhubRouter.RecycleLib,
  http_impl: SchoolhubRouterWeb.HttpMock

# Print only warnings and errors during test
config :logger, level: :warn
