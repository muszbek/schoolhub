defmodule Schoolhub.Auth do
  @moduledoc """
  Authentication server connecting with the database.
  """
  require Logger
  
  use GenServer

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
    GenServer.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  ### Server callbacks ###
  
  @impl true
  def init(:ok) do
    {:ok, %__MODULE__{}}
  end

  @impl true
  def handle_call({:auth, data}, from, state) do
    scram_data = :scramerl_lib.parse(data)
    Logger.debug("Auth server received data: #{inspect(scram_data, pretty: true)}")
    GenServer.cast(__MODULE__, {scram_data, from})
    {:noreply, state}
  end

  @impl true
  def handle_cast({data = %{message: 'client-first-message',
			    str: client_first}, from},
	state = %{db_api: db_api}) do
    
    {:ok, username} = Map.fetch(data, :username)
    {:ok, cnonce} = Map.fetch(data, :nonce)
    client_first_bare = :scramerl_lib.prune(:"gs2-header", client_first)
    
    scram_stored = db_api.get_scram_pw(username)
    scram_tokens = String.split(scram_stored, ",")
    ["==SCRAM==", stored_key, server_key, salt, iter_count] = scram_tokens

    snonce = :scramerl.gen_nonce()
    nonce = cnonce ++ snonce

    msg = :scramerl.server_first_message(charlist(nonce), charlist(salt), integer(iter_count))
    GenServer.reply(from, msg)
    {:noreply, %{state | client_first_bare: client_first_bare, server_first: msg,
		 stored_key: stored_key, server_key: server_key, nonce: nonce}}
  end

  @impl true
  def handle_cast({data = %{message: 'client-final-message',
			    nonce: nonce,
			    proof: proof}, from},
	state = %{client_first_bare: client_first_bare, server_first: server_first,
		  stored_key: stored_key, server_key: server_key, nonce: nonce}) do

    ## Nonce already verified via pattern matching! No need to explicitely do so.

    auth_msg = client_first_bare ++ ',' ++ server_first
    client_key = reproduce_client_key({stored_key, auth_msg, proof})
    Logger.debug(inspect(client_key))
    
    #msg = :scramerl.server_final_message(server_key)
    #GenServer.reply(from, msg)
    {:noreply, state}
  end

  
  ### Utility functions ###

  defp reproduce_client_key({stored_key, auth_msg, proof}) do
    client_signature_bin = :crypto.hmac(:sha, stored_key, auth_msg)
    client_signature = :base64.encode_to_string(client_signature_bin)
    client_key_bin = :crypto.exor(proof, client_signature)
    _client_key = :base64.encode_to_string(client_key_bin)
  end
  
  defp verify_credentials(cnonce, snonce, cproof, sproof) do
    :correct
  end

  defp string(text), do: text |> to_string()
  defp charlist(text), do: text |> to_charlist()

  defp integer(text) when is_integer(text), do: text
  defp integer(text), do: text |> string() |> String.to_integer()
  
  
end
