defmodule Schoolhub.DataManager do
  @moduledoc """
  API for accessing the Postgres database.
  """
  require Logger

  use GenServer

  ### API functions ###
  
  def start_link([]) do
    GenServer.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  ### Server callbacks ###
  
  @impl true
  def init(:ok) do
    {:ok, %{}}
  end
  
end
