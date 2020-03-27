defmodule Schoolhub.Auth do
  @moduledoc """
  Authentication server connecting with the database.
  """

  use GenServer

  ### API functions ###

  @doc false
  def start_link([]) do
    GenServer.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  ### Server callbacks ###
  def init(:ok) do
    {:ok, %{}}
  end
  
end
