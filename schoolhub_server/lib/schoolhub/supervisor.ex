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
      %{
	id: :mochiweb_socket_server,
	start: {:mochiweb_socket_server,
		:start_link, [[name: :mochiweb,
			       port: 8080,
			       loop: {Schoolhub.Router, :init_mochiweb, [[]]}]]}
      },
      Schoolhub.Auth
    ]

    opts = [strategy: :one_for_one]
    Supervisor.init(children, opts)
  end
  
end
