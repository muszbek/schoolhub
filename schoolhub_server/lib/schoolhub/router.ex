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
    Logger.debug("Router started...")
    {:ok, %{}}
  end

  def init_mochiweb(_socket, opts, args) do
    Logger.debug("Initialize Router. Opts: #{inspect(opts)}, Args: #{inspect(args)}")
    Process.register(self(), __MODULE__)
    :gen_server.enter_loop(__MODULE__, [], %{}, {:local, __MODULE__})
  end
  
end
