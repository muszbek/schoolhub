import Config

config :schoolhub_client,
  server_address: [scheme: :http,
		   ip: "localhost",
		   port: 8080],
  xmpp_backend: Romeo,
  mongooseim_hostname: "localhost"
