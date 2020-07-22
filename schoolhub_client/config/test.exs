import Config

config :schoolhub_client,
  server_opts: [scheme: :https,
		ip: "10.0.2.15",
		port: 8080,
		opts: [transport_opts: [verify: :verify_none]]],
  xmpp_backend: Client.RomeoMock
