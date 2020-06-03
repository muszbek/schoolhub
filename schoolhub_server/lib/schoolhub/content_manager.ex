defmodule Schoolhub.ContentManager do
  @moduledoc """
  API for accessing the Postgres database.
  Concerns course content, generic data are handled by Schoolhub.DataManager.
  """
  require Logger

  use GenServer

  defstruct(
    pgsql_conn: :nil
  )

  ### API functions ###

  @doc false
  def start_link([name: name]) do
    GenServer.start_link(name, :ok, name: __MODULE__)
  end

  @doc false
  def start_link([]) do
    GenServer.start_link(__MODULE__, :ok, name: __MODULE__)
  end


  ### Server callbacks ###
  
  @impl true
  def init(:ok) do
    args = Application.get_env(:schoolhub, :postgres_address)
    
    {:ok, pid} = Postgrex.start_link(args)
    {:ok, %__MODULE__{pgsql_conn: pid}}
  end


  ### Utility functions ###
  
  defp string(text), do: text |> to_string()

end
