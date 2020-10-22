defmodule Schoolhub.Router_mochiweb do
  @moduledoc """
  HTTP server and socket endpoint.
  """

  require Logger
  
  use GenServer

  defstruct socket: :nil
  

  ### API functions ###

  @doc false
  def start_link(_opts) do
    GenServer.start_link(__MODULE__, :ok, name: __MODULE__)
  end


  ### Server callbacks ###
  
  def init(:ok) do
    Logger.debug("Router started...")
    {:ok, %__MODULE__{}}
  end

  def init_mochiweb(socket, opts, args) do
    Logger.debug("Initialize Router through mochiweb. " <>
      "Opts: #{inspect(opts)}, Args: #{inspect(args)}")
    
    Process.register(self(), __MODULE__)
    state = %__MODULE__{socket: socket}
    :gen_server.enter_loop(__MODULE__, [], state, {:local, __MODULE__})
  end
  
end
