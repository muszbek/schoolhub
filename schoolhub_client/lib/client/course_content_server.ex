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
    body = %{course_name: course_name, description: desc |> pack_desc()}
    Rest.send_http_id(body, from, "PUT", "/courses/desc", state)
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

  defp pack_desc(desc) do
    case do_pack_desc(desc) do
      :json_decode_error -> %{text: desc |> string()}
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
