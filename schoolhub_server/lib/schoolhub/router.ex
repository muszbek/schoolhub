defmodule Schoolhub.Router do
  @moduledoc """
  HTTP server and socket endpoint.
  """

  use GenServer

  ### API functions ###

  @doc false
  def start_link(opts) do
    GenServer.start_link(__MODULE__, :ok, name: __MODULE__)
  end


  ### Server callbacks ###
  def init(:ok) do
    {:ok, %{}}
  end
  
end
