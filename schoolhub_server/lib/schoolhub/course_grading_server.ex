defmodule Schoolhub.CourseGradingServer do
  @moduledoc """
  GenServer submitting grades to students.
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

  
  defp pack_json(json) do
    case do_pack_json(json) do
      :json_decode_error -> %{text: json |> string()}
      :invalid -> nil
      map -> map
    end
  end

  defp jason_decode_catch(json) do
    try do
      Jason.decode!(json)
    rescue
      Jason.DecodeError -> :json_decode_error
    end
  end
  
  defp do_pack_json(json) when is_binary(json), do: json |> jason_decode_catch()
  defp do_pack_json(json) when is_list(json), do: json |> string() |> jason_decode_catch()
  defp do_pack_json(json = %{}), do: json
  defp do_pack_json(_json), do: :invalid
  
end
