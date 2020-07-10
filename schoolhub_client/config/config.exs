import Config

config :schoolhub_client,
  server_address: [scheme: :http,
		   ip: "localhost",
		   port: 8080],
  xmpp_backend: Romeo,
  xmpp_opts: [require_tls: true],
  mongooseim_hostname: "10.3.0.2"
