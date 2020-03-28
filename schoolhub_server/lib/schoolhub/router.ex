defmodule Schoolhub.Router do
  @moduledoc """
  HTTP server and socket endpoint.
  """

  require Logger
  
  use GenServer

  ### API functions ###

  @doc false
  def start_link(_opts) do
    GenServer.start_link(__MODULE__, :ok, name: __MODULE__)
  end


  ### Server callbacks ###
  def init(:ok) do
    Logger.debug("Rooter started...")
    {:ok, %{}}
  end

  def init(_socket, opts, args) do
    Logger.debug("Initialize Router. Opts: #{inspect(opts)}, Args: #{inspect(args)}")
    start_link(args)
  end
  
end
