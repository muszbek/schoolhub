defmodule AuthTest do

  use ExUnit.Case
  doctest Schoolhub.AuthStateMachine

  @client_first "n,,n=test_user,r=OXr<sUMqzqUr_mM"
  #@client_final "c=biws,r=OXr<sUMqzqUr_mMvLn;CrRX[bB^g@Z,p=cCVZHJsZbxSikIhPiElFF+cTCuA="

  #@server_first 'r=OXr<sUMqzqUr_mMvLn;CrRX[bB^g@Z,s=iCgKQkjMSgfZgjh06UMZzg==,i=4096'
  @server_final 'v=r+T1xjRnDwpUPoC/EwOXA+Jjt2Y='

  @server_first_start_match "r=OXr<sUMqzqUr_mM"
  @server_first_end_match ",s=iCgKQkjMSgfZgjh06UMZzg==,i=4096"
  @client_nonce "OXr<sUMqzqUr_mM"
  @salted_pw <<60, 60, 90, 18, 63, 251, 151, 24, 234, 108, 41, 221, 17, 92, 9, 52, 211, 193, 3, 72>>
  @auth_msg_start "n=test_user,r=OXr<sUMqzqUr_mM,r="
  @auth_msg_end ",s=iCgKQkjMSgfZgjh06UMZzg==,i=4096"

  @auth_server_name Schoolhub.AuthStateMachineTest

  setup do
    Schoolhub.AuthStateMachine.start_link([name: @auth_server_name,
					   db_api: Schoolhub.DataManagerMock])
    {:ok, []}
  end

  test "gets server first message" do
    response_first = GenServer.call(@auth_server_name, {:auth, @client_first})
    assert_client_first(response_first)
  end

  test "gets server final message" do
    response_first = GenServer.call(@auth_server_name, {:auth, @client_first})
    nonce = assert_client_first(response_first)

    client_final = get_client_final(nonce)
    response_final = GenServer.call(@auth_server_name, {:auth, client_final})
    assert @server_final == response_final
  end

  defp assert_client_first(response) do
    response_text = response |> to_string()
    assert @server_first_start_match <> <<snonce::bytes-size(15)>> <> @server_first_end_match =
      response_text
    _nonce = @client_nonce <> snonce
  end

  defp get_client_final(nonce) do
    auth_msg = @auth_msg_start <> nonce <> @auth_msg_end |> to_charlist()
    nonce = nonce |> to_charlist()
    _msg = :scramerl.client_final_message(nonce, @salted_pw, auth_msg)
  end
  

end
