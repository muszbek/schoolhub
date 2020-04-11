defmodule Schoolhub.AuthServer do
  @moduledoc """
  A supervisor that manages authentication sessions.
  A session is an instance of AuthStateMachine, that gets created upon request.
  """
  require Logger
  
  use Supervisor

  defstruct(
    db_api: Schoolhub.DataManagerMock
  )

  @auth_worker Schoolhub.AuthStateMachine
  @worker_options [db_api: Schoolhub.DataManagerMock]
  
  @nonce_length 15

  ### API functions ###
  
  @doc false
  def start_link([{:name, name} | options]) do
    Supervisor.start_link(__MODULE__, options, name: name)
  end
  
  def start_link(options) do
    Supervisor.start_link(__MODULE__, options, name: __MODULE__)
  end


  @doc false
  def authenticate(data) do
    requester = self()
    scram_data = handle_html_msg(data)
    handle_auth(scram_data, requester)
  end

  @impl true
  def init(options) do
    _init_struct = parse_options(options)
    children = []
    opts = [strategy: :one_for_one]
    Supervisor.init(children, opts)
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


  defp handle_html_msg(data) do
    scram_data = :scramerl_lib.parse(data)
    Logger.debug("Auth server received data: #{inspect(scram_data, pretty: true)}")
    scram_data
  end


  defp handle_auth(scram_data = %{message: 'client-first-message',
				  nonce: cnonce}, requester) do
    
    options = [name: session_name(cnonce),
	       requester: requester,
	       scram_data: scram_data] ++ @worker_options
    Supervisor.start_child(Schoolhub.AuthServer, @auth_worker.child_spec(options))
  end

  defp handle_auth(scram_data = %{message: 'client-final-message',
				  nonce: nonce}, requester) do

    session_name = nonce |> original_nonce() |> session_name()
    GenServer.cast(session_name, {:auth, scram_data, requester})
  end

  defp session_name(nonce) when is_list(nonce) do
    'auth_session_' ++ nonce |> to_string() |> String.to_atom()
  end
  defp session_name(nonce) do
    nonce |> to_charlist() |> session_name()
  end

  defp original_nonce(nonce) when is_list(nonce) do
    Enum.take(nonce, @nonce_length)
  end
  defp original_nonce(nonce) do
    nonce |> to_charlist() |> original_nonce()
  end
  
end
