import Config

config :schoolhub_client,
  server_address: [scheme: :https,
		   ip: "10.3.0.9",
		   port: 8080,
		   opts: [transport_opts: [verify: :verify_none]]],
  xmpp_backend: Romeo,
  mongooseim_hostname: "10.3.0.2"
