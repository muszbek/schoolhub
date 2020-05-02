defmodule Schoolhub.ChatServer do
  @moduledoc """
  Handles chat archive, link between REST request and database query.
  No direct XMPP connection.
  """
  require Logger

  use GenServer

  defstruct(
    db_api: Schoolhub.DataManagerMock
  )

  ### API functions ###
  
  @doc false
  def start_link(options) do
    GenServer.start_link(__MODULE__, options, name: __MODULE__)
  end
  
  @doc false
  def get_archive(self, partner) do
    GenServer.call(__MODULE__, {:get_archive, self, partner})
  end
  
  ### Server callbacks ###
  @impl true
  def init(options) do
    {:ok, parse_options(options)}
  end

  @impl true
  def handle_call({:get_archive, self, partner}, _from, state = %{db_api: db_api}) do
    archive = db_api.get_archive(self, partner)
    {:reply, archive, state}
  end

  
  ### Utility functions ###

  defp parse_options(state = %__MODULE__{}) do
    state
  end
  defp parse_options(options) do
    parse_options(options, %__MODULE__{})
  end
  defp parse_options([], state = %__MODULE__{}) do
    state
  end
  defp parse_options([{:db_api, db_api} | remaining_opts], state) do
    parse_options(remaining_opts, %{state | db_api: db_api})
  end
  defp parse_options([{_key, _value} | remaining_opts] ,state) do
    parse_options(remaining_opts, state)
  end
end
