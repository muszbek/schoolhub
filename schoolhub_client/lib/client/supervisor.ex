defmodule Client.Supervisor do
  @moduledoc """
  Root supervisor for client modules.
  Authentication and session supervisor.
  """
  require Logger

  use Supervisor

  @doc false
  def start_link() do
    Supervisor.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  @doc false
  def sup_start_session(username, password) do
    Supervisor.start_child(__MODULE__, {Client.SessionSup, {username, password}})
  end

  @doc false
  def sup_stop_session() do
    Supervisor.terminate_child(__MODULE__, Client.SessionSup)
    Supervisor.delete_child(__MODULE__, Client.SessionSup)
  end
  

  @impl true
  def init(:ok) do
    children = [
      {Client.Auth, server_address()},
      {Client.LoginServer, server_address()}
    ]

    opts = [strategy: :rest_for_one]
    Supervisor.init(children, opts)
  end

  defp server_address() do
    Application.get_env(:schoolhub_client, :server_address)
  end
  
end
