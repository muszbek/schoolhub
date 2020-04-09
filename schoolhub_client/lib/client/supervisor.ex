defmodule Client.Supervisor do
  @moduledoc """
  Root supervisor for client modules.
  Authentication and session supervisor.
  """
  require Logger

  use Supervisor

  def start_link() do
    Supervisor.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  @doc """
  Called by Client.Auth
  """
  def start_session() do
    Logger.debug("Starting session...")
    #Supervisor.start_child(__MODULE__, Client.Session.Supervisor)
  end
  

  @impl true
  def init(:ok) do
    children = [
      {Client.Auth, server_address()}
    ]

    opts = [strategy: :rest_for_one]
    Supervisor.init(children, opts)
  end

  defp server_address() do
    Application.get_env(:schoolhub_client, :server_address)
  end
  
end
