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
      {Schoolhub.AuthStateMachine, db_api: Schoolhub.DataManager},
      {Plug.Cowboy,
       scheme: server_scheme(), plug: Schoolhub.Router, options: [port: server_port()]}
    ]

    opts = [strategy: :one_for_one]
    Supervisor.init(children, opts)
  end


  defp server_scheme() do
    Application.get_env(:schoolhub, :server_scheme, :http)
  end

  defp server_port() do
    Application.get_env(:schoolhub, :server_port, 8080)
  end
  
end
