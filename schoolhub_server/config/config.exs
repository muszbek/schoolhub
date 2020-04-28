import Config

config :schoolhub,
  server_scheme: :http,
  server_port: 8080,
  db_backend: Schoolhub.DataManager,
  postgres_address: [hostname: "10.3.0.3",
		     username: "schoolhub",
		     password: "schoolhub",
		     database: "schoolhub"],
  xmpp_backend: Romeo,
  mongooseim_hostname: "10.3.0.2",
  reg_admin_name: "admin",

  auth_session_timeout: 1000
