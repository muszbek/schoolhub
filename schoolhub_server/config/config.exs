import Config

config :schoolhub,
  server_scheme: :http,
  server_port: 8080,

  postgres_address: [hostname: "10.3.0.3",
		     username: "schoolhub",
		     password: "schoolhub",
		     database: "schoolhub"],

  auth_session_timeout: 500
