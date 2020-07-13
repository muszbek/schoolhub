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
      Schoolhub.ContentManager,
      {Schoolhub.AuthServer, db_api: db_backend()},
      {Schoolhub.RegServer, db_api: db_backend(), xmpp_api: xmpp_backend()},
      {Schoolhub.ChatServer, db_api: db_backend()},
      {Schoolhub.CourseAdminServer, db_api: db_backend()},
      {Schoolhub.CourseContentServer, db_generic_api: db_backend(),
       db_content_api: db_content_backend()},
      {Schoolhub.CourseGradingServer, db_api: db_backend()},
      {Plug.Cowboy,
       scheme: server_scheme(), plug: Schoolhub.Router, options: server_opts()}
    ]

    opts = [strategy: :one_for_one]
    Supervisor.init(children, opts)
  end


  defp db_backend() do
    Application.get_env(:schoolhub, :db_backend, Schoolhub.DataManagerMock)
  end

  defp db_content_backend() do
    Application.get_env(:schoolhub, :db_content_backend, Schoolhub.ContentManagerMock)
  end

  defp xmpp_backend() do
    Application.get_env(:schoolhub, :xmpp_backend, Schoolhub.RomeoMock)
  end
  
  defp server_scheme() do
    Application.get_env(:schoolhub, :server_scheme, :http)
  end

  defp server_opts() do
    Application.get_env(:schoolhub, :server_opts, [port: 8080])
  end
  
end
