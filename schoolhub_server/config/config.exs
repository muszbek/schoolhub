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
  xmpp_opts: [require_tls: true, legacy_tls: true, host: "10.3.0.4"],
  mongooseim_hostname: "10.3.2.1",

  auth_session_timeout: 1000,

  admin_password: System.get_env("ADMIN_PASSWORD", "admin"),
  regger_password: System.get_env("REGGER_PASSWORD", "6z7r8h9i23l0ocgn")


config :libcluster,
  topologies: [
    schoolhub: [
      strategy: Cluster.Strategy.Epmd,
      config: [
	hosts: [:"server@10.3.1.1",
		:"server@10.3.1.2",
		:"server@10.3.1.3"]
      ]
    ]
  ]
