import Config

config :schoolhub,
  server_scheme: :https,
  server_opts: [port: 8080,
		otp_app: :schoolhub,
		cipher_suite: :strong,
		certfile: "priv/cert/schoolhub.crt",
		keyfile: "priv/cert/schoolhub.key"],
  db_backend: Schoolhub.DataManager,
  db_content_backend: Schoolhub.ContentManager,
  postgres_opts: [hostname: "10.3.0.3",
		  username: "schoolhub",
		  password: "schoolhub",
		  database: "schoolhub",
		  ssl: true,
		  ssl_opts: [verify: :verify_none]],
  xmpp_backend: Romeo,
  xmpp_opts: [require_tls: true],
  mongooseim_hostname: "10.3.0.2",

  auth_session_timeout: 1000

import_config "secret.exs"
