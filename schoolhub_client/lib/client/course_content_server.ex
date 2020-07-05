defmodule Client.CourseContentServer do
  @moduledoc """
  GenServer for general interaction on course functionalities.
  Some actions require specific privileges.
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
  Interrogates the database for the description field of the course.
  """
  def get_description(course_name) do
    GenServer.call(__MODULE__, {:get_desc, course_name})
  end
  
  @doc """
  Modifies the description field of the course.
  """
  def set_description(course_name, desc) do
    GenServer.call(__MODULE__, {:set_desc, course_name, desc})
  end


  @doc """
  Post a root level message on the course message board.
  """
  def post_message(course_name, message) do
    GenServer.call(__MODULE__, {:post_message, course_name, message})
  end

  @doc """
  Post an answer to an existing message.
  """
  def post_reply(id, course_name, message) do
    GenServer.call(__MODULE__, {:post_reply, id, course_name, message})
  end

  @doc """
  Retrieves the content of a single message as a json.
  """
  def get_single_message(id, course_name) do
    GenServer.call(__MODULE__, {:get_single_message, id, course_name})
  end

  @doc """
  Deletes a single message.
  If it had descendants originally, an empty placeholder is left in place of the message.
  Only author or course admins.
  """
  def delete_single_message(id, course_name) do
    GenServer.call(__MODULE__, {:delete_single_message, id, course_name})
  end

  @doc """
  Modifies a single message.
  Only author or course admins.
  """
  def modify_single_message(id, course_name, message) do
    GenServer.call(__MODULE__, {:modify_single_message, id, course_name, message})
  end


  @doc """
  Gets a number of root level messages, starting with the pinned ones.
  """
  def get_root_messages(course_name, number \\ 10) do
    GenServer.call(__MODULE__, {:get_root_messages, course_name, number})
  end

  @doc """
  Gets a number of replies for the specified root message.
  """
  def get_replies(id, course_name, number \\ 10) do
    GenServer.call(__MODULE__, {:get_replies, id, course_name, number})
  end

  @doc """
  Removes a root message together with all replies.
  """
  def delete_root_message(id, course_name) do
    GenServer.call(__MODULE__, {:delete_root_message, id, course_name})
  end

  @doc """
  Sets the pinned flag of the root message.
  Iff pinned is true, the message is pushed to top in order.
  """
  def pin_message(id, course_name, pinned \\ true) do
    GenServer.call(__MODULE__, {:pin_message, id, course_name, pinned})
  end


  @doc """
  Reading out the grades of a student.
  Other students not permitted.
  """
  def get_grades(course_name, target) do
    GenServer.call(__MODULE__, {:get_grades, course_name, target})
  end

  @doc """
  Setting the grades of a student in json format.
  """
  def set_grades(course_name, target, grades) do
    GenServer.call(__MODULE__, {:set_grades, course_name, target, grades})
  end

  @doc """
  Reads out the grades of a student as json, and merges that with the supplied grades.
  """
  def append_grades(course_name, target, grades) do
    GenServer.call(__MODULE__, {:append_grades, course_name, target, grades})
  end

  @doc """
  Setting the grades of many students at once from a list.
  List elements have to be: {student, grades}
  """
  def mass_set_grades(course_name, grade_list) do
    GenServer.call(__MODULE__, {:mass_set_grades, course_name, grade_list})
  end

  @doc """
  Appending to the grades of many students at once from a list.
  List elements have to be: {student, grades}
  Appending under a common key, if given.
  """
  def mass_append_grades(course_name, grade_list) do
    GenServer.call(__MODULE__, {:mass_append_grades, course_name, grade_list})
  end
  def mass_append_grades(course_name, grade_list, key) do
    GenServer.call(__MODULE__, {:mass_append_grades, course_name, grade_list, key})
  end


  ### Server callbacks ###
  @impl true
  def init(options) do
    state = parse_options(options)
    {:ok, state}
  end

  @impl true
  def handle_call({:get_desc, course_name}, from, state) do
    body = %{course_name: course_name}
    Rest.send_http(body, from, "GET", "/courses/desc", state)
  end

  @impl true
  def handle_call({:set_desc, course_name, desc}, from, state) do
    body = %{course_name: course_name, description: desc |> pack_json()}
    Rest.send_http_id(body, from, "PUT", "/courses/desc", state)
  end

  
  @impl true
  def handle_call({:post_message, course_name, message}, from, state) do
    body = %{id: nil, course_name: course_name, message: message |> pack_json()}
    Rest.send_http_id(body, from, "POST", "/courses/messages", state)
  end

  @impl true
  def handle_call({:post_reply, id, course_name, message}, from, state) do
    body = %{id: id, course_name: course_name, message: message |> pack_json()}
    Rest.send_http_id(body, from, "POST", "/courses/messages", state)
  end
  
  @impl true
  def handle_call({:get_single_message, id, course_name}, from, state) do
    body = %{id: id, course_name: course_name, number: 1}
    Rest.send_http_id(body, from, "GET", "/courses/messages", state)
  end

  @impl true
  def handle_call({:delete_single_message, id, course_name}, from, state) do
    body = %{id: id, course_name: course_name, all: false}
    Rest.send_http_id(body, from, "DELETE", "/courses/messages", state)
  end

  @impl true
  def handle_call({:modify_single_message, id, course_name, message}, from, state) do
    body = %{id: id, course_name: course_name, message: message |> pack_json()}
    Rest.send_http_id(body, from, "PUT", "/courses/messages", state)
  end

  @impl true
  def handle_call({:get_root_messages, course_name, number}, from, state) do
    body = %{course_name: course_name, number: number}
    Rest.send_http_id(body, from, "GET", "/courses/message_board", state)
  end

  @impl true
  def handle_call({:get_replies, id, course_name, number}, from, state) do
    body = %{id: id, course_name: course_name, number: number}
    Rest.send_http_id(body, from, "GET", "/courses/messages", state)
  end

  @impl true
  def handle_call({:delete_root_message, id, course_name}, from, state) do
    body = %{id: id, course_name: course_name, all: true}
    Rest.send_http_id(body, from, "DELETE", "/courses/messages", state)
  end

  @impl true
  def handle_call({:pin_message, id, course_name, pinned}, from, state) do
    body = %{id: id, course_name: course_name, pinned: pinned}
    Rest.send_http_id(body, from, "PUT", "/courses/messages/pin", state)
  end

  @impl true
  def handle_call({:get_grades, course_name, target}, from, state) do
    body = %{course_name: course_name, target: target}
    Rest.send_http_id(body, from, "GET", "/courses/students/grades", state)
  end

  @impl true
  def handle_call({:set_grades, course_name, target, grades}, from, state) do
    body = %{course_name: course_name, target: target, grades: grades}
    Rest.send_http_id(body, from, "POST", "/courses/students/grades", state)
  end

  @impl true
  def handle_call({:append_grades, course_name, target, grades}, from, state) do
    body = %{course_name: course_name, target: target, grades: grades}
    Rest.send_http_id(body, from, "PUT", "/courses/students/grades", state)
  end

  @impl true
  def handle_call({:mass_set_grades, course_name, grade_list}, from, state) do
    body = %{course_name: course_name, grade_list: grade_list}
    Rest.send_http_id(body, from, "POST", "/courses/students/grades/mass", state)
  end

  @impl true
  def handle_call({:mass_append_grades, course_name, grade_list}, from, state) do
    body = %{course_name: course_name, grade_list: grade_list, key: nil}
    Rest.send_http_id(body, from, "PUT", "/courses/students/grades/mass", state)
  end

  @impl true
  def handle_call({:mass_append_grades, course_name, grade_list, key}, from, state) do
    body = %{course_name: course_name, grade_list: grade_list, key: key}
    Rest.send_http_id(body, from, "PUT", "/courses/students/grades/mass", state)
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
