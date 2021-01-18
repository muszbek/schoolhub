# In this file, we load production configuration and secrets
# from environment variables. You can also hardcode secrets,
# although such is generally not recommended and you have to
# remember to add this file to your .gitignore.
use Mix.Config

config :schoolhub, Schoolhub.Repo,
  username: System.get_env("POSTGRES_USER", "schoolhub"),
  password: System.get_env("POSTGRES_PASSWORD", "schoolhub"),
  database: System.get_env("POSTGRES_DB", "schoolhub"),
  hostname: System.get_env("POSTGRES_HOST", "10.3.0.3"),
  ssl: true,
  ssl_opts: [cacertfile: "priv/cert/chain.pem"],
  pool_size: String.to_integer(System.get_env("POOL_SIZE") || "10")

secret_key_base =
  System.get_env("SECRET_KEY_BASE",
    "/d8p4MwXOA3uA9jZ2fIouO5BwvwOIRM9Gqny3Cf7XFaY/pQ1Yukf53H9iy9AugqP") ||
  raise """
  environment variable SECRET_KEY_BASE is missing.
  You can generate one by calling: mix phx.gen.secret
  """

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
  ],
  secret_key_base: secret_key_base

# ## Using releases (Elixir v1.9+)
#
# If you are doing OTP releases, you need to instruct Phoenix
# to start each relevant endpoint:
#
#     config :schoolhub, SchoolhubWeb.Endpoint, server: true
#
# Then you can assemble a release by calling `mix release`.
# See `mix help release` for more information.
