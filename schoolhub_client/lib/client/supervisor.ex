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

  end

  @impl true
  def init(:ok) do
    children = [
      Client.Auth
    ]

    opts = [strategy: :rest_for_one]
    Supervisor.init(children, opts)
  end
  
end
