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
	{Client.ChatServer, {username, password}},
	{Client.ChatArchiveServer, [username: username] ++ server_address()},
	{Client.AdminServer, [username: username] ++ server_address()},
	{Client.CourseAdminServer, [username: username] ++ server_address()},
	{Client.CourseContentServer, [username: username] ++ server_address()}
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

  
  defp server_address() do
    Application.get_env(:schoolhub_client, :server_opts)
  end
end
