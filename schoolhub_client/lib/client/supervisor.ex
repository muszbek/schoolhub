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

  @doc false
  def start_session(username, password) do
    auth_result = Client.Auth.auth(username, password)
    
    if auth_result == :authenticated do
      Supervisor.start_child(__MODULE__, {Client.SessionSup, {username, password}})
      Logger.debug("Starting session...")
    end
	
    auth_result
  end

  @doc false
  def end_session() do
    Logger.debug("Ending session...")
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
