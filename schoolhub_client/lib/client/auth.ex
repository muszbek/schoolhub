defmodule Client.Auth do
  @moduledoc """
  Authentication client module.
  Triggers session start.
  """
  require Logger

  use GenServer

  defstruct(
    scheme: :http,
    ip: "localhost",
    port: 8080,
    conn: :nil
  )
    
  ### API functions ###

  @doc false
  def start_link([]) do
    GenServer.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  @doc false
  def auth(username, password) do
    
  end

  ### Server callbacks ###
  @impl true
  def init(:ok) do
    state = %__MODULE__{}
    
    
    {:ok, state}
  end

  @impl true
  def handle_call({:client_first}, _from, state) do
    {:ok, conn} = Mint.HTTP.connect(state.scheme, state.ip, state.port)
    
    {:reply, :ok, %{state | conn: conn}}
  end
  
end
