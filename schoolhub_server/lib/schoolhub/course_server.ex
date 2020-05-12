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

  @doc """
  Interrogates the database to check affiliation of user to course.
  'student' -> restricted privileges, default
  'assistant' -> able to modify course, invite new students
  'owner' -> able to modify course, invite new student, change affiliation
  """
  def get_affiliation(user, course_name) do
    GenServer.call(__MODULE__, {:get_affiliation, user, course_name})
  end

  @doc """
  Changes the affiliation of the target user with the specified course.
  Only works if the target is already invited to the course.
  """
  def set_affiliation(self, target, course_name, affiliation) do
    GenServer.call(__MODULE__, {:set_affiliation, self, target, course_name, string(affiliation)})
  end

  @doc """
  Makes the user affiliated with the specified course.
  """
  def invite_student(self, target, course_name) do
    GenServer.call(__MODULE__, {:invite_student, self, target, course_name})
  end

  @doc """
  Removes the affiliation entry from the database related to the specified course.
  """
  def remove_student(self, target, course_name) do
    GenServer.call(__MODULE__, {:remove_student, self, target, course_name})
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
	       "teacher" -> do_create_course(course_name, owner, db_api)
	       "admin" -> do_create_course(course_name, owner, db_api)
	     end
    {:reply, result, state}
  end

  @impl true
  def handle_call({:remove_course, self, course_name}, _from, state = %{db_api: db_api}) do
    result = case do_get_affiliation(self, course_name, db_api) do
	       "owner" ->
		 do_remove_course(course_name, db_api)

	       err = {:error, :course_not_exist} ->
		 err
	       
	       _other_aff ->
		 case Schoolhub.RegServer.get_user_privilege(self) do
		   "admin" ->
		     do_remove_course(course_name, db_api)
		   
		   _other_priv ->
		     {:error, :no_permission}
		 end
	     end
		 
    {:reply, result, state}
  end

  @impl true
  def handle_call({:get_affiliation, user, course_name}, _from, state = %{db_api: db_api}) do
    affiliation = do_get_affiliation(user, course_name, db_api)
    {:reply, affiliation, state}
  end

  @impl true
  def handle_call({:set_affiliation, self, target, course_name, aff}, _from,
	state = %{db_api: db_api}) do

    result = do_set_affiliation(self, target, course_name, aff, db_api)
    {:reply, result, state}
  end

  @impl true
  def handle_call({:invite_student, self, target, course_name}, _from,
	state = %{db_api: db_api}) do
    
    result = case can_i_admin_course(self, course_name, db_api) do
	       :ok -> db_api.invite_student(target, course_name)
	       err -> err
	     end
    
    {:reply, result, state}
  end

  @impl true
  def handle_call({:remove_student, self, target, course_name}, _from,
	state = %{db_api: db_api}) do
    
    result = case can_i_admin_course(self, course_name, db_api) do
	       :ok -> db_api.remove_student(target, course_name)
	       err -> err
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

  
  defp do_create_course(course_name, owner, db_api) do
    case course_name |> charlist() |> :stringprep.prepare() do
      '' ->
	{:error, :name_invalid}
      name_checked ->
	db_api.create_course(name_checked, owner)
    end
  end

  defp do_get_affiliation(user, course_name, db_api) do
    db_api.get_affiliation(user, course_name)
  end

  defp do_remove_course(name, db_api) do
    db_api.remove_course(name)
  end

  defp check_affiliation("owner"), do: {:ok, "owner"}
  defp check_affiliation("assistant"), do: {:ok, "assistant"}
  defp check_affiliation("student"), do: {:ok, "student"}
  defp check_affiliation(_other), do: {:error, :wrong_affiliation}

  defp can_i_change_affiliation("owner"), do: :ok
  defp can_i_change_affiliation({:error, reason}), do: {:error, reason}
  defp can_i_change_affiliation(_other), do: {:error, :no_permission}

  defp can_i_change_affiliation(user, course_name, db_api) do
    case Schoolhub.RegServer.get_user_privilege(user) do
      "admin" -> :ok
      _other -> do_get_affiliation(user, course_name, db_api) |> can_i_change_affiliation()
    end
  end
  
  defp can_i_admin_course("owner"), do: :ok
  defp can_i_admin_course("assistant"), do: :ok
  defp can_i_admin_course({:error, reason}), do: {:error, reason}
  defp can_i_admin_course(_other), do: {:error, :no_permission}

  defp can_i_admin_course(user, course_name, db_api) do
    case Schoolhub.RegServer.get_user_privilege(user) do
      "admin" -> :ok
      _other -> do_get_affiliation(user, course_name, db_api) |> can_i_admin_course()
    end
  end

  defp do_set_affiliation(self, target, course_name, aff, db_api) do
    ## Last with clause is to check if target is affiliated to begin with.
    _result = with :ok <- can_i_change_affiliation(self, course_name, db_api),
                   {:ok, ^aff} <- aff |> check_affiliation()
              do
                db_api.set_affiliation(target, course_name, aff)
	      else
                err = {:error, _reason} -> err
              end
  end
  
end
