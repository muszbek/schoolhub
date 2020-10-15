defmodule Schoolhub.Accounts.ScramLib do
  @moduledoc """
  Library module for encoding password into the database.
  It is conforming to the implementation in Mongooseim, 
  in order to authenticate with the same credentials.
  """

  alias Ecto.Changeset
  
  @scram_default_iteration_count 10000
  @scram_serial_prefix "==MULTI_SCRAM=="
  @salt_length 16
  @sha_methods [:sha, :sha224, :sha256, :sha384, :sha512]
  
  def encode_password_in_changeset(changeset) do
    encoded_pw = encode_password(changeset)

    changeset
    |> Changeset.put_change(:pass_details, encoded_pw)
    |> Changeset.put_change(:password, "")
  end

  def encode_password(password) when is_binary(password) do
    password
    |> encode_password()
    |> to_string()
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
    prefix = @scram_serial_prefix <> "," <> to_string(iteration_count)
    Enum.reduce(@sha_methods, prefix,
      fn x, acc -> acc <> "," <> password_to_scram(password, iteration_count, x) end)
  end
  
  defp password_to_scram(password, iteration_count, hash_type) do
    salt = :crypto.strong_rand_bytes(@salt_length)
    server_stored_keys = password_to_scram(password, salt, iteration_count, hash_type)
    result_list = server_stored_keys ++ [salt: :base64.encode(salt), hash_type: hash_type]
    serialize(Enum.into(result_list, %{}))
  end
  
  defp password_to_scram(password, salt, iteration_count, hash_type) do
    salted_password = salted_password(hash_type, password, salt, iteration_count)
    stored_key = stored_key(hash_type, client_key(hash_type, salted_password))
    server_key = server_key(hash_type, salted_password)
    
    [stored_key: :base64.encode(stored_key),
     server_key: :base64.encode(server_key)]
  end

  defp salted_password(hash_type, password, salt, iteration_count) do
    normalized_pw = :stringprep.prepare(password)
    _salted_pw = hi(normalized_pw, salt, iteration_count, hash_type)
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

  defp serialize(%{stored_key: stored_key,
		   server_key: server_key,
		   salt: salt,
		   hash_type: hash_type}) do
    
    sha_prefix(hash_type) <> salt <> "|" <> stored_key <> "|" <> server_key
  end


  ## source: https://github.com/pundunlabs/scramerl/blob/master/src/scramerl_lib.erl
  defp hi(str, salt, i, sha) when is_list(str), do: hi(to_string(str), salt, i, sha)
  defp hi(str, salt, i, sha) when is_list(salt), do: hi(str, to_string(salt), i, sha)
  defp hi(str, salt, 1, sha), do: :crypto.hmac(sha, str, <<salt :: binary, 0, 0, 0, 1>>)
  defp hi(str, salt, i, sha) when is_integer(i) and i > 1 do
    u1 = :crypto.hmac(sha, str, <<salt :: binary, 0, 0, 0, 1>>)
    hi(str, [u1], i, 1, sha)
  end

  defp hi(str, [uy], i, 1, sha) do
    ux = :crypto.hmac(sha, str, uy)
    hi(str, [ux, uy], i, 2, sha)
  end
  defp hi(_str, [uy, uz], i , i, _sha), do: :crypto.exor(uy, uz)
  defp hi(str, [uy, uz], i, n, sha) do
    ux = :crypto.hmac(sha, str, uy)
    exor = :crypto.exor(uy, uz)
    hi(str, [ux, exor], i, n+1, sha)
  end

  defp sha_prefix(:sha), do: "===SHA1==="
  defp sha_prefix(:sha224), do: "==SHA224=="
  defp sha_prefix(:sha256), do: "==SHA256=="
  defp sha_prefix(:sha384), do: "==SHA384=="
  defp sha_prefix(:sha512), do: "==SHA512=="
  
end
