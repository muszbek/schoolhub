defmodule Schoolhub.Application do
  # See https://hexdocs.pm/elixir/Application.html
  # for more information on OTP Applications
  @moduledoc false

  use Application

  def start(_type, _args) do
    children = [
      # Start the Ecto repository
      Schoolhub.Repo,
      # Start the Telemetry supervisor
      SchoolhubWeb.Telemetry,
      # Start the PubSub system
      {Phoenix.PubSub, name: Schoolhub.PubSub},
      # Start the Endpoint (http/https)
      SchoolhubWeb.Endpoint,
      # Start a worker by calling: Schoolhub.Worker.start_link(arg)
      # {Schoolhub.Worker, arg}
      Schoolhub.AuthServer
    ]

    # See https://hexdocs.pm/elixir/Supervisor.html
    # for other strategies and supported options
    opts = [strategy: :one_for_one, name: Schoolhub.Supervisor]
    Supervisor.start_link(children, opts)
  end

  # Tell Phoenix to update the endpoint configuration
  # whenever the application is updated.
  def config_change(changed, _new, removed) do
    SchoolhubWeb.Endpoint.config_change(changed, removed)
    :ok
  end
end
