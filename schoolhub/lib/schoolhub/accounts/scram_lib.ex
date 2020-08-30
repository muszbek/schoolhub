defmodule Schoolhub.Accounts.ScramLib do
  @moduledoc """
  Library module for encoding password into the database.
  It is conforming to the implementation in Mongooseim, 
  in order to authenticate with the same credentials.
  """

  alias Ecto.Changeset
  
  @scram_default_iteration_count 4096
  @scram_serial_prefix "==SCRAM==,"
  @salt_length 16
  
  def encode_password_in_changeset(changeset) do
    encoded_pw = encode_password(changeset)

    changeset
    |> Changeset.put_change(:pass_details, encoded_pw)
    |> Changeset.put_change(:password, "")
  end
  
  def encode_password(password) when is_list(password) do
    password_to_scram(password, @scram_default_iteration_count)
  end
  def encode_password(changeset) do
    changeset
    |> Changeset.get_field(:password)
    |> to_charlist()
    |> encode_password()
  end

  ## Ported from erlang code of mongooseim
  
  defp password_to_scram(password, iteration_count) do
    salt = :crypto.strong_rand_bytes(@salt_length)
    server_stored_keys = password_to_scram(password, salt, iteration_count, :sha)
    result_list = server_stored_keys ++ [salt: :base64.encode(salt),
					 iteration_count: iteration_count |> to_string()]
    serialize(Enum.into(result_list, %{}))
  end
  
  defp password_to_scram(password, salt, iteration_count, hash_type) do
    salted_password = salted_password(hash_type, password, salt, iteration_count)
    stored_key = stored_key(hash_type, client_key(hash_type, salted_password))
    server_key = server_key(hash_type, salted_password)
    
    [server_key: :base64.encode(server_key),
     stored_key: :base64.encode(stored_key)]
  end

  defp salted_password(_hash_type, password, salt, iteration_count) do
    normalized_pw = :stringprep.prepare(password)
    _salted_pw = :scramerl_lib.hi(normalized_pw, salt, iteration_count)
  end

  defp client_key(hash_type, salted_password) do
    :crypto.hmac(hash_type, salted_password, "Client Key")
  end
  
  defp stored_key(hash_type, client_key) do
    :crypto.hash(hash_type, client_key)
  end

  defp server_key(hash_type, salted_password) do
    :crypto.hmac(hash_type, salted_password, "Server Key")
  end

  defp serialize(%{server_key: server_key,
		   stored_key: stored_key,
		   salt: salt,
		   iteration_count: ic}) do
    
    @scram_serial_prefix <> stored_key <> "," <> server_key <> "," <> salt <> "," <> ic
  end
  
end
