defmodule Schoolhub.Supervisor do
  @moduledoc """
  Root supervisor for server modules.
  """
  require Logger
  
  use Supervisor

  def start_link() do
    Supervisor.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  @impl true
  def init(:ok) do
    children = [
#      %{
#	id: :mochiweb_socket_server,
#	start: {:mochiweb_socket_server,
#		:start_link, [[name: :mochiweb,
#			       port: 8081,
#			       loop: {Schoolhub.Router_mochiweb, :init_mochiweb, [[]]}]]}
#      },
      Schoolhub.DataManager,
      {Schoolhub.Auth, db_api: Schoolhub.DataManager},
      {Plug.Cowboy, scheme: :http, plug: Schoolhub.Router, options: [port: 8080]}
    ]

    opts = [strategy: :one_for_one]
    Supervisor.init(children, opts)
  end
  
end
