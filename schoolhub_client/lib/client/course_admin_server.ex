defmodule Client.CourseAdminServer do
  @moduledoc """
  GenServer for administrative actions on courses.
  Not all users have access to these functionalities.
  """
  require Logger

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
  

  ### Server callbacks ###
  @impl true
  def init(options) do
    state = parse_options(options)
    {:ok, state}
  end

  
  @impl true
  def handle_call({:create_course, course_name}, from, state = %{username: self,
								 scheme: scheme,
								 ip: ip,
								 port: port}) do
    msg = Jason.encode!(%{self: self, course_name: course_name})
    {:ok, conn} = Mint.HTTP.connect(scheme, ip, port)
    {:ok, conn, _request_ref} = Mint.HTTP.request(conn, "GET", "/create_course", [], msg)
    
    {:noreply, %{state |
		 conn: conn,
		 socket: conn.socket,
		 reg_caller: from}}
  end

  @impl true
  def handle_call({:remove_course, course_name}, from, state = %{username: self,
								 scheme: scheme,
								 ip: ip,
								 port: port}) do
    msg = Jason.encode!(%{self: self, course_name: course_name})
    {:ok, conn} = Mint.HTTP.connect(scheme, ip, port)
    {:ok, conn, _request_ref} = Mint.HTTP.request(conn, "GET", "/remove_course", [], msg)
    
    {:noreply, %{state |
		 conn: conn,
		 socket: conn.socket,
		 reg_caller: from}}
  end

  @impl true
  def handle_call({:get_affiliation, course_name}, from, state = %{username: self,
								   scheme: scheme,
								   ip: ip,
								   port: port}) do
    msg = Jason.encode!(%{user: self, course_name: course_name, get_all: false})
    {:ok, conn} = Mint.HTTP.connect(scheme, ip, port)
    {:ok, conn, _request_ref} = Mint.HTTP.request(conn, "GET", "/get_affiliation", [], msg)
    
    {:noreply, %{state |
		 conn: conn,
		 socket: conn.socket,
		 reg_caller: from}}
  end
  

  @impl true
  def handle_info({transport, socket, http_response}, state = %{conn: conn,
								socket: socket,
								reg_caller: from}) do

    {:ok, _conn, response} = Mint.HTTP.stream(conn, {transport, socket, http_response})
    {:data, _ref, data_json}  = :lists.keyfind(:data, 1, response)
    data = Jason.decode!(data_json)
    
    GenServer.reply(from, data)
    {:noreply, state}
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
