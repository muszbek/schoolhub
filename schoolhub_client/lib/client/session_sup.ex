defmodule Client.SessionSup do
  @moduledoc """
  Session root for client application, created once client has initially authenticated.
  """
  require Logger

  use Supervisor

  def start_link(args) do
    Supervisor.start_link(__MODULE__, args, name: __MODULE__)
  end

  @impl true
  def init({username, password}) do
    auth_check = Client.Auth.auth(username, password)
    
    if auth_check == :authenticated do
      children = [
	{Client.ChatServer, {username, password}}
      ]
      opts = [strategy: :one_for_one]
      Supervisor.init(children, opts)
    else
      Logger.debug("Session authentication verification failed...")
      :ignore
    end
  end

  def child_spec(args) do
    %{
      id: __MODULE__,
      start: {__MODULE__, :start_link, [args]},
      restart: :transient,
      type: :supervisor
    }
  end
end
