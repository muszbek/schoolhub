import Config

config :schoolhub,
  server_scheme: :http,
  server_port: 8080,
  db_backend: Schoolhub.DataManager,
  postgres_address: [hostname: "10.3.0.3",
		     username: "schoolhub",
		     password: "schoolhub",
		     database: "schoolhub"],

  auth_session_timeout: 1000,

  reg_admin_jid: "admin@localhost",
  reg_admin_pw: "admin"
