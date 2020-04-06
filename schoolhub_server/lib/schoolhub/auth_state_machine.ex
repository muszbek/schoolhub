defmodule Schoolhub.AuthStateMachine do
  @moduledoc """
  Authentication server connecting with the database.
  Implemented as an erlang gen_statem.
  """
  require Logger

  @behaviour :gen_statem

  defstruct(
    db_api: Schoolhub.DataManager,
    client_first_bare: '',
    server_first: '',
    stored_key: '',
    server_key: '',
    nonce: ''
  )

  ### API functions ###

  @doc false
  def start_link(_args) do
    :gen_statem.start_link({:local, __MODULE__}, __MODULE__, :ok, [])
  end

  
  ### Server callbacks ###
  
  @impl true
  def init(:ok) do
    {:ok, :idle, %__MODULE__{}}
  end

  @impl true
  def callback_mode() do
    :state_functions
  end

  
  def idle({:call, from}, {:auth, data}, state) do
    scram_data = handle_html_msg(data)
    
    {:next_state, :client_first, state,
     [{:next_event, :internal, {scram_data, from}}]}
  end

  
  def client_first(:internal, {%{message: 'client-first-message',
				 nonce: cnonce,
				 username: username,
				 str: client_first}, from},
	state = %{db_api: db_api}) do

    client_first_bare = :scramerl_lib.prune(:"gs2-header", client_first)
    scram_stored = db_api.get_scram_pw(username)
    scram_tokens = String.split(scram_stored, ",")
    ["==SCRAM==", stored_key, server_key, salt, iter_count] = scram_tokens

    snonce = :scramerl.gen_nonce()
    nonce = cnonce ++ snonce

    msg = :scramerl.server_first_message(charlist(nonce), charlist(salt), integer(iter_count))
    :gen_statem.reply(from, msg)
    
    {:next_state, :server_first, %{state |
				   client_first_bare: client_first_bare,
				   server_first: msg,
				   stored_key: stored_key,
				   server_key: server_key,
				   nonce: nonce}}
  end

  
  def server_first({:call, from}, {:auth, data}, state) do
    scram_data = handle_html_msg(data)
    
    {:next_state, :client_final, state,
     [{:next_event, :internal, {scram_data, from}}]}
  end

  
  def client_final(:internal, {%{message: 'client-final-message',
				 nonce: nonce,
				 proof: proof}, from},
	state = %{client_first_bare: client_first_bare,
		  server_first: server_first,
		  stored_key: stored_key,
		  server_key: server_key,
		  nonce: nonce}) do

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

    finish_authentication(msg, from, state)
  end

  def client_final(:internal, {%{message: 'client-final-message',
				 nonce: _nonce}, from},
	state = %{nonce: _other_nonce}) do

    msg =
      {:error, 'nonce_mismatch'}
      |> :scramerl.server_final_message()

    finish_authentication(msg, from, state)
  end


  def server_final(:enter, :client_final, state) do
    ## This state is only for decoration, to represent the scram flow.
    {:next_state, :idle, state |> reset_state()}
  end

  
  def child_spec(opts) do
    %{
      id: __MODULE__,
      start: {__MODULE__, :start_link, [opts ++ [name: __MODULE__]]},
      type: :worker,
      restart: :permanent,
      shutdown: 500
    }
  end


  ### Utility functions ###

  defp string(text), do: text |> to_string()
  defp charlist(text), do: text |> to_charlist()

  defp integer(text) when is_integer(text), do: text
  defp integer(text), do: text |> string() |> String.to_integer()

  defp handle_html_msg(data) do
    scram_data = :scramerl_lib.parse(data)
    Logger.debug("Auth server received data: #{inspect(scram_data, pretty: true)}")
    scram_data
  end
  
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
    end
  end

  defp reset_state(state) do
    %{state |
      client_first_bare: '',
      server_first: '',
      stored_key: '',
      server_key: '',
      nonce: ''}
  end
  
  defp finish_authentication(msg, from, state) do
    :gen_statem.reply(from, msg)
    {:next_state, :server_final, state, [:next_event, :internal, []]}
  end
  
end
