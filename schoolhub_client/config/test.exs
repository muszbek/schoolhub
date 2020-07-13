import Config

config :schoolhub_client,
  server_opts: [scheme: :https,
		ip: "schoolhub",
		port: 8080,
		opts: [transport_opts: [verify: :verify_none]]],
  xmpp_backend: Client.RomeoMock
