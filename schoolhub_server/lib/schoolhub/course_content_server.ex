defmodule Schoolhub.CourseContentServer do
  @moduledoc """
  GenServer creating and modifying content associated to courses.
  Course description, message wall, files.
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


  ### Server callbacks ###
  @impl true
  def init(options) do
    {:ok, parse_options(options)}
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

  
  defp charlist(text), do: text |> to_charlist()
  defp string(text), do: text |> to_string()
  
end
