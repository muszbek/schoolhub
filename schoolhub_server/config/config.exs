import Config

config :schoolhub,
  server_scheme: :http,
  server_port: 8080,
  db_backend: Schoolhub.DataManager,
  db_content_backend: Schoolhub.ContentManager,
  postgres_opts: [hostname: "10.3.0.3",
		  username: "schoolhub",
		  password: "schoolhub",
		  database: "schoolhub",
		  ssl: true,
		  ssl_opts: [verify: :verify_none,
			     cacertfile: "../ssl/schoolhub_cacert.crt"]],
  xmpp_backend: Romeo,
  mongooseim_hostname: "10.3.0.2",

  auth_session_timeout: 1000

import_config "secret.exs"
