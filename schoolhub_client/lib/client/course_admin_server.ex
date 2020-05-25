defmodule Client.CourseAdminServer do
  @moduledoc """
  GenServer for administrative actions on courses.
  Not all users have access to these functionalities.
  """
  require Logger
  alias Client.RestLib, as: Rest

  use GenServer

  defstruct(
    username: :nil,
    scheme: :http,
    ip: "localhost",
    port: 8080,
    conn: :nil,
    socket: :nil,
    reg_caller: :nil
  )

  ### API functions ###

  @doc false
  def start_link(options) do
    GenServer.start_link(__MODULE__, options, name: __MODULE__)
  end

  @doc """
  Create course with the given name.
  Only with admin or teacher privilege.
  """
  def create_course(course_name) do
    GenServer.call(__MODULE__, {:create_course, course_name})
  end

  @doc """
  Remove course with the given name.
  Only with admin privilege, or being the owner of the course.
  """
  def remove_course(course_name) do
    GenServer.call(__MODULE__, {:remove_course, course_name})
  end

  @doc """
  Interrogates the database to check affiliation of user to course.
  'student' -> restricted privileges, default
  'assistant' -> able to modify course, invite new students
  'owner' -> able to modify course, invite new student, change affiliation
  """
  def get_affiliation(course_name) do
    GenServer.call(__MODULE__, {:get_affiliation, course_name})
  end

  @doc """
  Interrogates the database to return an affiliation list of all users to course.
  """
  def get_all_affiliation(course_name) do
    GenServer.call(__MODULE__, {:get_all_affiliation, course_name})
  end

  @doc """
  Updates the affiliation of target user in the specified course, if it is affiliated already.
  Only with admin privilege, or being the owner of the course.
  """
  def set_affiliation(user, course_name, affiliation) do
    GenServer.call(__MODULE__, {:set_affiliation, user, course_name, affiliation})
  end

  @doc """
  Makes the target user affiliated to the specified course, if it exists.
  """
  def invite_student(user, course_name) do
    GenServer.call(__MODULE__, {:invite_student, user, course_name})
  end

  @doc """
  Removes the affiliation of target user from the specified course.
  """
  def remove_student(user, course_name) do
    GenServer.call(__MODULE__, {:remove_student, user, course_name})
  end
  

  ### Server callbacks ###
  @impl true
  def init(options) do
    state = parse_options(options)
    {:ok, state}
  end

  
  @impl true
  def handle_call({:create_course, course_name}, from, state) do
    body = %{course_name: course_name}
    Rest.send_http_id(body, from, "PUT", "/create_course", state)
  end

  @impl true
  def handle_call({:remove_course, course_name}, from, state) do
    body = %{course_name: course_name}
    Rest.send_http_id(body, from, "PUT", "/remove_course", state)
  end

  @impl true
  def handle_call({:get_affiliation, course_name}, from, state) do
    body = %{course_name: course_name, get_all: false}
    Rest.send_http_id(body, from, "GET", "/get_affiliation", state)
  end

  @impl true
  def handle_call({:get_all_affiliation, course_name}, from, state) do
    body = %{course_name: course_name, get_all: true}
    Rest.send_http_id(body, from, "GET", "/get_affiliation", state)
  end

  @impl true
  def handle_call({:set_affiliation, target, course_name, aff}, from, state) do
    body = %{target: target, course_name: course_name, affiliation: aff}
    Rest.send_http_id(body, from, "PUT", "/set_affiliation", state)
  end

  @impl true
  def handle_call({:invite_student, target, course_name}, from, state) do
    body = %{target: target, course_name: course_name}
    Rest.send_http_id(body, from, "PUT", "/invite_student", state)
  end

  @impl true
  def handle_call({:remove_student, target, course_name}, from, state) do
    body = %{target: target, course_name: course_name}
    Rest.send_http_id(body, from, "PUT", "/remove_student", state)
  end
  

  @impl true
  def handle_info(http_info = {_transport, _socket, _http_response}, state) do
    Rest.receive_http(http_info, state)
  end

  @impl true
  def handle_info({:tcp_closed, socket}, state = %{socket: socket}) do
    Logger.debug("TCP closed for socket #{inspect(socket)}")
    {:noreply, %{state |
		 conn: :nil,
		 socket: :nil}}
  end

  @impl true
  def handle_info({:tcp_closed, socket}, state = %{socket: _other_socket}) do
    ## This message does not affect the current registration session.
    Logger.debug("TCP closed for socket #{inspect(socket)}")
    {:noreply, state}
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
  defp parse_options([{:username, username} | remaining_opts], state) do
    parse_options(remaining_opts, %{state | username: username})
  end
  defp parse_options([{:scheme, scheme} | remaining_opts], state) do
    parse_options(remaining_opts, %{state | scheme: scheme})
  end
  defp parse_options([{:ip, ip} | remaining_opts], state) do
    parse_options(remaining_opts, %{state | ip: ip})
  end
  defp parse_options([{:port, port} | remaining_opts], state) do
    parse_options(remaining_opts, %{state | port: port})
  end
  defp parse_options([{_key, _value} | remaining_opts] ,state) do
    parse_options(remaining_opts, state)
  end
  
  defp string(text), do: text |> to_string()
  
end
