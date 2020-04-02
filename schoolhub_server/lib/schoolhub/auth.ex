defmodule Schoolhub.Auth do
  @moduledoc """
  Authentication server connecting with the database.
  """
  require Logger
  
  use GenServer

  defstruct(
    db_api: Schoolhub.DataManager
  )

  ### API functions ###

  @doc false
  def start_link(_args) do
    GenServer.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  ### Server callbacks ###
  
  @impl true
  def init(:ok) do
    {:ok, %{}}
  end

  @impl true
  def handle_call({:auth, data}, from, state) do
    scram_data = :scramerl_lib.parse(data)
    Logger.debug("Auth server received data: #{inspect(scram_data, pretty: true)}")
    GenServer.cast(__MODULE__, {scram_data, from})
    {:noreply, state}
  end

  @impl true
  def handle_cast({data = %{message: 'client-first-message'}, from},
	state = %{db_api: db_api}) do
    
    {:ok, username} = Map.fetch(data, :username)
    {:ok, cnonce} = Map.fetch(data, :nonce)
    
    scram_stored = db_api.get_scram_pw(username)
    scram_tokens = String.split(scram_stored, ",")
    ["==SCRAM==", _stored_key, _server_key, salt, iter_count] = scram_tokens

    snonce = :scramerl.gen_nonce()
    nonce = cnonce ++ snonce

    msg = :scramerl.server_first_message(charlist(nonce), charlist(salt), integer(iter_count))
    GenServer.reply(from, msg)
    {:noreply, state}
  end

  
  ### Utility functions ###

  defp string(text), do: text |> to_string()
  defp charlist(text), do: text |> to_charlist()

  defp integer(text) when is_integer(text), do: text
  defp integer(text), do: text |> string() |> String.to_integer()
  
  
end
