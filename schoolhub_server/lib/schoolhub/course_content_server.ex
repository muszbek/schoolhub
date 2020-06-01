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

  @doc """
  Interrogates the database for the description field of the course.
  """
  def get_description(course_name) do
    GenServer.call(__MODULE__, {:get_desc, course_name})
  end
  
  @doc """
  Modifies the description field of the course.
  """
  def set_description(self, course_name, desc) do
    GenServer.call(__MODULE__, {:set_desc, self, course_name, desc})
  end


  ### Server callbacks ###
  @impl true
  def init(options) do
    {:ok, parse_options(options)}
  end

  @impl true
  def handle_call({:get_desc, course_name}, _from,
	state = %{db_api: db_api}) do
    
    result = db_api.get_course_desc(course_name)
    {:reply, result, state}
  end

  @impl true
  def handle_call({:set_desc, self, course_name, desc}, _from,
	state = %{db_api: db_api}) do
    
    result = case Schoolhub.CourseAdminServer.can_i_admin_course(self, course_name) do
	       :ok -> db_api.set_course_desc(course_name, desc |> pack_desc())
	       err = {:error, _reason} -> err
	     end
    
    {:reply, result, state}
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

  
  defp pack_desc(desc) do
    case do_pack_desc(desc) do
      :json_decode_error -> desc |> string()
      :invalid -> nil
      map -> map
    end
  end

  defp jason_decode_catch(desc) do
    try do
      Jason.decode!(desc)
    rescue
      Jason.DecodeError -> :json_decode_error
    end
  end
  
  defp do_pack_desc(desc) when is_binary(desc), do: desc |> jason_decode_catch()
  defp do_pack_desc(desc) when is_list(desc), do: desc |> string() |> jason_decode_catch()
  defp do_pack_desc(desc = %{}), do: desc
  defp do_pack_desc(_desc), do: :invalid
  
end
