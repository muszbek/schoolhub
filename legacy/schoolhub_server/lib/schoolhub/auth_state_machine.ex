defmodule Schoolhub.AuthStateMachine do
  @moduledoc """
  Authentication server connecting with the database.
  Implemented as an erlang gen_statem.
  """
  require Logger

  @behaviour :gen_statem

  defstruct(
    db_api: Schoolhub.DataManagerMock,
    requester: :nil,
    client_first_bare: '',
    server_first: '',
    stored_key: '',
    server_key: '',
    nonce: ''
  )

  ### API functions ###

  @doc false
  def start_link([{:name, name} | options]) do
    :gen_statem.start_link({:local, name}, __MODULE__, options, [])
  end

  def start_link(options) do
    :gen_statem.start_link({:local, __MODULE__}, __MODULE__, options, [])
  end

  
  ### Server callbacks ###
  
  @impl true
  def init(options) do
    find_scram_data = fn
      {:scram_data, data} -> data
      {_, _} -> false
    end
    scram_data = Enum.find_value(options, find_scram_data)
    
    {:ok, :client_first, parse_options(options),
     [{:next_event, :internal, scram_data}]}
  end

  @impl true
  def callback_mode() do
    :state_functions
  end

  
  def client_first(:internal, %{message: 'client-first-message',
				nonce: cnonce,
				username: username,
				str: client_first},
	state = %{db_api: db_api,
		  requester: from}) do

    client_first_bare = :scramerl_lib.prune(:"gs2-header", client_first)
    
    case db_api.get_scram_pw(username) do
      :nil ->
	msg = 'unknown_user'

	send(from, {:reply, msg})
        {:stop, :unknown_user_error}
      
      scram_stored ->
	scram_tokens = String.split(scram_stored, ",")
	["==SCRAM==", stored_key, server_key, salt, iter_count] = scram_tokens
	
	snonce = :scramerl.gen_nonce()
	nonce = cnonce ++ snonce
	
	msg = :scramerl.server_first_message(charlist(nonce), charlist(salt), integer(iter_count))

	session_timeout = Application.get_env(:schoolhub, :auth_session_timeout)
	send(from, {:reply, msg})
	{:next_state, :server_first, %{state |
				       client_first_bare: client_first_bare,
				       server_first: msg,
				       stored_key: stored_key,
				       server_key: server_key,
				       nonce: nonce},
	 [{:state_timeout, session_timeout, []}]}
    end
  end


  def server_first(:cast, {:auth, scram_data = %{message: 'client-final-message'}, from},
	state) do

    {:next_state, :client_final, %{state | requester: from},
     [{:next_event, :internal, scram_data}]}
  end

  def server_first(:state_timeout, [], _state = %{username: username}) do
    Logger.info("Session to authenticate #{inspect(username)} timed out...")
    {:stop, :client_final_timeout}
  end

  
  def client_final(:internal, %{message: 'client-final-message',
				nonce: nonce,
				proof: proof},
	_state = %{client_first_bare: client_first_bare,
		   server_first: server_first,
		   stored_key: stored_key,
		   server_key: server_key,
		   nonce: nonce,
		   requester: from}) do

    ## Nonce already verified via pattern matching! No need to explicitely do so.
  
    proof = :base64.decode(proof)
    auth_msg = client_first_bare ++ ',' ++ server_first
  
    stored_key_from_client =
      {stored_key |> :base64.decode(), auth_msg, proof}
      |> reproduce_client_key()
      |> reproduce_stored_key()
    
    msg =
      {stored_key_from_client, stored_key, server_key}
      |> verify_credentials()
      |> :scramerl.server_final_message()

    finish_authentication(msg, from, :normal)
  end

  def client_final(:internal, {%{message: 'client-final-message',
				 nonce: _nonce}, from},
	_state = %{nonce: _other_nonce}) do

    msg =
      {:error, 'nonce_mismatch'}
      |> :scramerl.server_final_message()
    ## Meaning: someone inpersonating the client

    finish_authentication(msg, from, :auth_error)
  end

  
  def child_spec(opts = [{:name, name} | _rest_opts]) do
    %{
      id: name,
      start: {__MODULE__, :start_link, [opts]},
      type: :worker,
      restart: :temporary,
      shutdown: 500
    }
  end

  def child_spec(opts) do
    child_spec(opts ++ [name: __MODULE__])
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
  defp parse_options([{:requester, requester} | remaining_opts], state) do
    parse_options(remaining_opts, %{state | requester: requester})
  end
  defp parse_options([{:scram_data, _data} | remaining_opts], state) do
    parse_options(remaining_opts, state)
  end
  defp parse_options([{_key, _value} | remaining_opts] ,state) do
    parse_options(remaining_opts, state)
  end
  

  defp string(text), do: text |> to_string()
  defp charlist(text), do: text |> to_charlist()

  defp integer(text) when is_integer(text), do: text
  defp integer(text), do: text |> string() |> String.to_integer()

  
  defp reproduce_client_key({stored_key, auth_msg, proof}) do
    client_signature = :crypto.hmac(:sha, stored_key, auth_msg)
    _client_key = :crypto.exor(proof, client_signature)
  end

  defp reproduce_stored_key(client_key) do
    _stored_key = :crypto.hash(:sha, client_key)
      |> :base64.encode()
  end
  
  defp verify_credentials({ckey, skey, server_key}) do
    if ckey == skey do
      server_key |> charlist()
    else
      {:error, 'stored_key_mismatch'}
      ## Meaning: wrong password
    end
  end
  
  defp finish_authentication(msg, from, reason) do
    send(from, {:reply, msg})
    
    {:stop, reason}
  end
  
end
