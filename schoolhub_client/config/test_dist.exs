import Config

config :schoolhub_client,
  server_opts: [scheme: :https,
		ip: "10.3.0.9",
		port: 8080,
		opts: [transport_opts: [verify: :verify_none]]],
  xmpp_backend: Romeo,
  xmpp_opts: [require_tls: true, legacy_tls: true, host: "10.3.0.4"],
  mongooseim_hostname: "10.3.2.1"
