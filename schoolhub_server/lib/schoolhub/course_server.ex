defmodule Schoolhub.CourseServer do
  @moduledoc """
  GenServer creating and modifying courses, handling member affiliations.
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
  Creates course if the acting user has a privilege teacher or bigger.
  """
  def create_course(self, course_name) do
    GenServer.call(__MODULE__, {:create_course, self, course_name})
  end

  @doc """
  Removes course with all affiliations.
  The requester user has to be either the owner of this course, or a global admin.
  """
  def remove_course(self, course_name) do
    GenServer.call(__MODULE__, {:remove_course, self, course_name})
  end


  ### Server callbacks ###
  @impl true
  def init(options) do
    {:ok, parse_options(options)}
  end

  @impl true
  def handle_call({:create_course, owner, course_name}, _from, state = %{db_api: db_api}) do
    result = case Schoolhub.RegServer.get_user_privilege(owner) do
	       "student" -> {:error, :no_permission}
	       "teacher" -> create_course(course_name, owner, db_api)
	       "admin" -> create_course(course_name, owner, db_api)
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

  defp string(text), do: text |> to_string()
  defp charlist(text), do: text |> to_charlist()

  defp create_course(course_name, owner, db_api) do
    case course_name |> charlist() |> :stringprep.prepare() do
      '' ->
	{:error, :name_invalid}
      name_checked ->
	db_api.create_course(name_checked, owner)
    end
  end
  
end
