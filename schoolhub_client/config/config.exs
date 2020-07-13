import Config

config :schoolhub_client,
  server_opts: [scheme: :https,
		ip: "schoolhub",
		port: 8080,
		opts: [transport_opts: [verify: :verify_peer,
					cacertfile: "priv/cert/schoolhub_cacert.crt"]]],
  xmpp_backend: Romeo,
  xmpp_opts: [require_tls: true],
  mongooseim_hostname: "10.3.0.2"
