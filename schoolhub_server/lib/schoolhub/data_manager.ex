defmodule Schoolhub.DataManager do
  @moduledoc """
  API for accessing the Postgres database.
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

  @doc """
  Fetching the scram data entry associated with username in the database.
  The scram data entry is a composition of different hashed values, decoded in Schoolhub.Auth.
  """
  def get_scram_pw(username) do
    GenServer.call(__MODULE__, {:get_pw, username})
  end
    

  ### Server callbacks ###
  
  @impl true
  def init(:ok) do
    args = Application.get_env(:schoolhub, :postgres_address)
    
    {:ok, pid} = Postgrex.start_link(args)
    {:ok, %__MODULE__{pgsql_conn: pid}}
  end

  @impl true
  def handle_call({:get_pw, username}, _from, state = %{pgsql_conn: conn}) do
    query_text = "SELECT pass_details FROM users WHERE username LIKE $1"
    {:ok, data} = Postgrex.query(conn, query_text, [string(username)])

    case data.rows do
      [] ->
	Logger.debug("#{inspect(username)} is not present in database")
        {:reply, :nil, state}
      [[result]] ->
	Logger.debug("Fetching scram data from database for username #{inspect(username)}: "
	  <> "#{inspect(result)}")
        {:reply, result, state}
    end
  end

  
  ### Utility functions ###
  
  defp string(text), do: text |> to_string()
  
end
