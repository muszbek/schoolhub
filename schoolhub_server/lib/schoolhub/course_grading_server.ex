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

  @doc """
  Reading out the grades of a student.
  Other students not permitted.
  """
  def get_grades(self, course_name, target) do
    GenServer.call(__MODULE__, {:get_grades, self, course_name, target})
  end

  @doc """
  Setting the grades of a student in json format.
  """
  def set_grades(self, course_name, target, grades) do
    GenServer.call(__MODULE__, {:set_grades, self, course_name, target, grades})
  end

  @doc """
  Reads out the grades of a student as json, and merges that with the supplied grades.
  """
  def append_grades(self, course_name, target, grades) do
    GenServer.call(__MODULE__, {:merge_grades, self, course_name, target, grades})
  end
  
  
  ### Server callbacks ###
  @impl true
  def init(options) do
    {:ok, parse_options(options)}
  end

  @impl true
  def handle_call({:get_grades, self, course_name, self}, _from,
	state = %{db_api: db_api}) do

    result = db_api.get_grades(course_name, self)
    {:reply, result, state}
  end
  
  @impl true
  def handle_call({:get_grades, self, course_name, target}, _from,
	state = %{db_api: db_api}) do

    result = case Schoolhub.CourseAdminServer.can_i_admin_course(self, course_name) do
	       :ok -> db_api.get_grades(course_name, target)
	       err = {:error, _reason} -> err
	     end
    {:reply, result, state}
  end

  @impl true
  def handle_call({:set_grades, self, course_name, target, grades}, _from,
	state = %{db_api: db_api}) do

    result = case Schoolhub.CourseAdminServer.can_i_admin_course(self, course_name) do
	       :ok -> db_api.set_grades(course_name, target, grades |> pack_json())
	       err = {:error, _reason} -> err
	     end
    {:reply, result, state}
  end

  @impl true
  def handle_call({:merge_grades, self, course_name, target, grades}, _from,
	state = %{db_api: db_api}) do

    result = with :ok <- Schoolhub.CourseAdminServer.can_i_admin_course(self, course_name),
                  old_grades <- db_api.get_grades(course_name, target)
             do
	       new_grades = Map.merge(old_grades, grades |> pack_json())
	       Logger.debug("Updating grades of #{target}: #{inspect(new_grades)}")
	       db_api.set_grades(course_name, target, new_grades)
	     else
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

  
  defp pack_json(json) do
    case do_pack_json(json) do
      :json_decode_error -> %{"total" => json |> string()}
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
  defp do_pack_json(other), do: %{"total" => other}
  
end
