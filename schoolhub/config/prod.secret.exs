# In this file, we load production configuration and secrets
# from environment variables. You can also hardcode secrets,
# although such is generally not recommended and you have to
# remember to add this file to your .gitignore.
use Mix.Config

config :schoolhub, Schoolhub.Repo,
  username: System.get_env("POSTGRES_USER", "schoolhub"),
  password: System.get_env("POSTGRES_PASSWORD", "schoolhub"),
  database: System.get_env("POSTGRES_DB", "schoolhub"),
  hostname: System.get_env("POSTGRES_HOST", "postgres"),
  ssl: true,
  ssl_opts: [cacertfile: "priv/cert/chain.pem"],
  pool_size: String.to_integer(System.get_env("POOL_SIZE") || "10")

config :schoolhub, SchoolhubWeb.Endpoint,
  http: [
    port: String.to_integer(System.get_env("PORT") || "4000"),
    transport_options: [socket_opts: [:inet6]]
  ],
  https: [
    port: String.to_integer(System.get_env("PORT_TLS") || "4001"),
    cipher_suite: :strong,
    keyfile: "priv/cert/privkey.pem",
    certfile: "priv/cert/fullchain.pem",
    transport_options: [socket_opts: [:inet6]]
  ]

# ## Using releases (Elixir v1.9+)
#
# If you are doing OTP releases, you need to instruct Phoenix
# to start each relevant endpoint:
#
#     config :schoolhub, SchoolhubWeb.Endpoint, server: true
#
# Then you can assemble a release by calling `mix release`.
# See `mix help release` for more information.
