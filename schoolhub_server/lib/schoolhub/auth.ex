defmodule Schoolhub.Auth do
  @moduledoc """
  Authentication server connecting with the database.
  """
  require Logger
  
  use GenServer

  defstruct(
    db_api: Schoolhub.DataManager
  )

  ### API functions ###

  @doc false
  def start_link(_args) do
    GenServer.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  ### Server callbacks ###
  
  @impl true
  def init(:ok) do
    {:ok, %{}}
  end

  @impl true
  def handle_call({:auth, data}, _from, state) do
    scram_data = :scramerl_lib.parse(data)
    Logger.debug("Auth server received data: #{inspect(scram_data, pretty: true)}")
    GenServer.cast(__MODULE__, scram_data)
    {:reply, "this_is_my_response", state}
  end

  @impl true
  def handle_cast(data = %{message: 'client-first-message'}, state) do
    {:ok, username} = Map.fetch(data, :username)
    Logger.debug(inspect(username))
    {:noreply, state}
  end
  
  
end
