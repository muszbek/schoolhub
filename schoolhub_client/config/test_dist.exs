import Config

config :schoolhub_client,
  server_address: [scheme: :http,
		   ip: "10.3.0.9",
		   port: 8080],
  xmpp_backend: Romeo,
  mongooseim_hostname: "10.3.0.2"
