defmodule SchoolhubRouterWeb.Endpoint do
  use Phoenix.Endpoint, otp_app: :schoolhub_router

  # The session will be stored in the cookie and signed,
  # this means its contents can be read but not tampered with.
  # Set :encryption_salt if you would also like to encrypt it.
  @session_options [
    domain: System.get_env("DOMAIN", "localhost"),
    store: :cookie,
    secure: true,
    key: "_schoolhub_key",
    signing_salt: System.get_env("SESSION_SIGNING_SALT", "ygUkyJkL"),
    encryption_salt: System.get_env("SESSION_CRYPT_SALT", "Xx5sjPQJ")
  ]

  socket "/socket", SchoolhubRouterWeb.UserSocket,
    websocket: true,
    longpoll: false

  socket "/live", Phoenix.LiveView.Socket, websocket: [connect_info: [session: @session_options]]

  # Serve at "/" the static files from "priv/static" directory.
  #
  # You should set gzip to true if you are running phx.digest
  # when deploying your static files in production.
  plug Plug.Static,
    at: "/",
    from: :schoolhub_router,
    gzip: false,
    only: ~w(css fonts images js favicon.ico robots.txt)

  # Code reloading can be explicitly enabled under the
  # :code_reloader configuration of your endpoint.
  if code_reloading? do
    socket "/phoenix/live_reload/socket", Phoenix.LiveReloader.Socket
    plug Phoenix.LiveReloader
    plug Phoenix.CodeReloader
    plug Phoenix.Ecto.CheckRepoStatus, otp_app: :schoolhub_router
  end

  plug Phoenix.LiveDashboard.RequestLogger,
    param_key: "request_logger",
    cookie_key: "request_logger"

  plug Plug.RequestId
  plug Plug.Telemetry, event_prefix: [:phoenix, :endpoint]

  plug Stripe.WebhookPlug,
    at: "/router/webhook/stripe",
    handler: SchoolhubRouterWeb.StripeHandler,
    secret: Application.get_env(:stripity_stripe, :webhook_secret)

  plug Plug.Parsers,
    parsers: [:urlencoded, :multipart, :json],
    pass: ["*/*"],
    json_decoder: Phoenix.json_library()

  plug Plug.MethodOverride
  plug Plug.Head
  plug Plug.Session, @session_options
  plug SchoolhubRouterWeb.Router
end
