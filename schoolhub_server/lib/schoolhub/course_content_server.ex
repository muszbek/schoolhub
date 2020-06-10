defmodule Schoolhub.CourseContentServer do
  @moduledoc """
  GenServer creating and modifying content associated to courses.
  Course description, message wall, files.
  """
  require Logger

  use GenServer

  defstruct(
    db_generic_api: Schoolhub.DataManagerMock,
    db_content_api: Schoolhub.ContentManagerMock
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

  @doc """
  Post a root level message on the course message board.
  """
  def post_message(self, course_name, message) do
    GenServer.call(__MODULE__, {:post_message, self, course_name, message})
  end

  @doc """
  Post an answer to an existing message.
  """
  def post_reply(id, self, course_name, message) do
    GenServer.call(__MODULE__, {:post_reply, id, self, course_name, message})
  end

  @doc """
  Retrieves the content of a single message as a json.
  """
  def get_single_message(id, self, course_name) do
    GenServer.call(__MODULE__, {:get_single_message, id, self, course_name})
  end

  @doc """
  Deletes a single message.
  If it had descendants originally, an empty placeholder is left in place of the message.
  Only author or course admins.
  """
  def delete_single_message(id, self, course_name) do
    GenServer.call(__MODULE__, {:delete_single_message, id, self, course_name})
  end

  @doc """
  Modifies a single message.
  Only author or course admins.
  """
  def modify_single_message(id, self, course_name, message) do
    GenServer.call(__MODULE__, {:modify_single_message, id, self, course_name, message})
  end
  

  ### Server callbacks ###
  @impl true
  def init(options) do
    {:ok, parse_options(options)}
  end

  @impl true
  def handle_call({:get_desc, course_name}, _from,
	state = %{db_generic_api: db_api}) do
    
    result = db_api.get_course_desc(course_name)
    {:reply, result, state}
  end

  @impl true
  def handle_call({:set_desc, self, course_name, desc}, _from,
	state = %{db_generic_api: db_api}) do
    
    result = case Schoolhub.CourseAdminServer.can_i_admin_course(self, course_name) do
	       :ok -> db_api.set_course_desc(course_name, desc |> pack_json())
	       err = {:error, _reason} -> err
	     end
    
    {:reply, result, state}
  end

  @impl true
  def handle_call({:post_message, self, course_name, message}, _from,
	state = %{db_content_api: db_api}) do

    result = case am_i_affiliated(self, course_name) do
	       err = {:error, _reason} -> err
	       {:ok, _aff} -> db_api.post_message(self, course_name, message |> pack_json())
	     end

    {:reply, result, state}
  end

  @impl true
  def handle_call({:post_reply, id, self, course_name, message}, _from,
	state = %{db_content_api: db_api}) do

    result = case am_i_affiliated(self, course_name) do
	       err = {:error, _reason} -> err
	       {:ok, _aff} -> db_api.post_reply(id, self, course_name, message |> pack_json())
	     end

    {:reply, result, state}
  end


  @impl true
  def handle_call({:get_single_message, id, self, course_name}, _from,
	state = %{db_content_api: db_api}) do

    result = case am_i_affiliated(self, course_name) do
	       err = {:error, _reason} -> err
	       {:ok, _aff} -> db_api.get_single_message(id, course_name)
	     end
    {:reply, result, state}
  end

  @impl true
  def handle_call({:delete_single_message, id, self, course_name}, _from,
	state = %{db_content_api: db_api}) do

    result = case can_i_modify_message(id, self |> string(), course_name |> string(), db_api) do
	       err = {:error, _reason} -> err
	       :ok -> db_api.delete_single_message(id, course_name)
	     end
    {:reply, result, state}
  end

  @impl true
  def handle_call({:modify_single_message, id, self, course_name, message}, _from,
	state = %{db_content_api: db_api}) do

    result = case can_i_modify_message(id, self |> string(), course_name |> string(), db_api) do
	       err = {:error, _reason} -> err
	       :ok ->
		 db_api.modify_single_message(id, course_name, self, message |> pack_json())
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
  defp parse_options([{:db_generic_api, db_api} | remaining_opts], state) do
    parse_options(remaining_opts, %{state | db_generic_api: db_api})
  end
  defp parse_options([{:db_content_api, db_api} | remaining_opts], state) do
    parse_options(remaining_opts, %{state | db_content_api: db_api})
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

  defp am_i_affiliated({:error, reason}), do: {:error, reason}
  defp am_i_affiliated(aff), do: {:ok, aff}

  defp am_i_affiliated(user, course_name) do
    case Schoolhub.RegServer.get_user_privilege(user) do
      "admin" -> {:ok, "admin"}
      _other -> Schoolhub.CourseAdminServer.get_affiliation(user, course_name)
	|> am_i_affiliated()
    end
  end

  defp can_i_modify_message(id, user, course_name, db_api) do
    case Schoolhub.CourseAdminServer.can_i_admin_course(user, course_name) do
      :ok -> :ok
      {:error, :no_permission} ->
	case db_api.get_single_message(id, course_name) do
	  err = {:error, _reason} -> err
	  %{author: ^user} -> :ok
	  %{author: _other} -> {:error, :no_permission}
	end
      
      err = {:error, _other} -> err
    end
  end
  
end
