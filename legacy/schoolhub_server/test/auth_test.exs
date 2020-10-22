defmodule AuthTest do

  use ExUnit.Case
  doctest Schoolhub.AuthServer

  #@client_first "n,,n=test_user,r=OXr<sUMqzqUr_mM"
  #@client_final "c=biws,r=OXr<sUMqzqUr_mMvLn;CrRX[bB^g@Z,p=cCVZHJsZbxSikIhPiElFF+cTCuA="

  #@server_first 'r=OXr<sUMqzqUr_mMvLn;CrRX[bB^g@Z,s=iCgKQkjMSgfZgjh06UMZzg==,i=4096'
  @server_final 'v=r+T1xjRnDwpUPoC/EwOXA+Jjt2Y='

  @username 'test_user'
  @client_first_start_match "n,,n=test_user,r="
  @server_first_start_match "r="
  @server_first_end_match ",s=iCgKQkjMSgfZgjh06UMZzg==,i=4096"
  @salted_pw <<60, 60, 90, 18, 63, 251, 151, 24, 234, 108, 41, 221, 17, 92, 9, 52, 211, 193, 3, 72>>
  @auth_msg_start "n=test_user,r="
  @auth_msg_mid ",r="
  @auth_msg_end ",s=iCgKQkjMSgfZgjh06UMZzg==,i=4096"

  @auth_server_name Schoolhub.AuthServer
  @assert_receive_timeout 100

  setup do
    kill_session = fn
      {id, _pid, :worker, [Schoolhub.AuthStateMachine]} ->
	Supervisor.terminate_child(@auth_server_name, id)
      {_id, _pid, _type, [_other_module]} ->
	:ok
    end
    all_sessions = Supervisor.which_children(@auth_server_name)
    Enum.map(all_sessions, kill_session)
    {:ok, []}
  end

  
  test "gets server first message" do
    auth_and_assert_first()
  end

  test "gets server final message" do
    nonce = auth_and_assert_first()

    auth_and_assert_final(nonce)
  end

  test "succeeds two consequtive sessions" do
    nonce = auth_and_assert_first()

    auth_and_assert_final(nonce)

    nonce2 = auth_and_assert_first()

    auth_and_assert_final(nonce2)
  end

  test "succeeds one intersecting session" do
    nonce = auth_and_assert_first()

    _nonce2 = auth_and_assert_first()

    auth_and_assert_final(nonce)
  end
  
  test "succeeds two intersecting sessions" do
    nonce = auth_and_assert_first()

    nonce2 = auth_and_assert_first()

    auth_and_assert_final(nonce)
    
    auth_and_assert_final(nonce2)
  end

  test "fails because nonce mismatch" do
    nonce = auth_and_assert_first()

    client_final = get_client_final_wrong_nonce(nonce)
    Schoolhub.AuthServer.authenticate(client_final)
    assert_server_final_fail_or_timeout()
  end

  test "fails because wrong password" do
    nonce = auth_and_assert_first()

    client_final = get_client_final_wrong_pw(nonce)
    Schoolhub.AuthServer.authenticate(client_final)
    assert_server_final_fail()
  end

  test "session times out" do
    timeout = Application.get_env(:schoolhub, :auth_session_timeout)
    auth_and_assert_first()
    :timer.sleep(timeout + 50)
    assert [{Schoolhub.AuthServerState, _pid, :worker, [Schoolhub.AuthServerState]}] =
      Supervisor.which_children(@auth_server_name)
  end


  defp auth_and_assert_first() do
    {cnonce, client_first} = get_client_first(@username)
    Schoolhub.AuthServer.authenticate(client_first)
    _nonce = assert_server_first(cnonce)
  end

  defp auth_and_assert_final(nonce) do
    client_final = get_client_final(nonce)
    Schoolhub.AuthServer.authenticate(client_final)
    assert_server_final()
  end

  
  defp get_client_first(username) do
    msg = username |> to_charlist |> :scramerl.client_first_message() |> to_string()
    @client_first_start_match <> <<cnonce::bytes-size(15)>> = msg
    {cnonce, msg}
  end
  
  defp assert_server_first(cnonce) do
    assert_receive {:reply, response}, @assert_receive_timeout
    assert @server_first_start_match <> <<^cnonce::bytes-size(15)>>
  <> <<snonce::bytes-size(15)>> <> @server_first_end_match =
      response |> to_string()
    _nonce = cnonce <> snonce
  end

  defp get_client_final(nonce) do
    <<cnonce::bytes-size(15)>> <> <<_snonce::bytes-size(15)>> = nonce
    auth_msg = @auth_msg_start <> cnonce <> @auth_msg_mid <> nonce <> @auth_msg_end
      |> to_charlist()
    nonce = nonce |> to_charlist()
    _msg = :scramerl.client_final_message(nonce, @salted_pw, auth_msg)
  end

  defp get_client_final_wrong_nonce(nonce) do
    <<cnonce::bytes-size(15)>> <> <<_snonce::bytes-size(15)>> = nonce
    auth_msg = @auth_msg_start <> cnonce <> @auth_msg_mid <> nonce <> @auth_msg_end
      |> to_charlist()
    nonce = nonce |> to_charlist() |> Enum.reverse()
    _msg = :scramerl.client_final_message(nonce, @salted_pw, auth_msg)
  end

  defp get_client_final_wrong_pw(nonce) do
    <<cnonce::bytes-size(15)>> <> <<_snonce::bytes-size(15)>> = nonce
    auth_msg = @auth_msg_start <> cnonce <> @auth_msg_mid <> nonce <> @auth_msg_end
      |> to_charlist()
    nonce = nonce |> to_charlist()
    _msg = :scramerl.client_final_message(nonce, @salted_pw <> <<99>>, auth_msg)
  end

  defp assert_server_final() do
    assert_receive {:reply, response}, @assert_receive_timeout
    assert @server_final == response
  end

  defp assert_server_final_fail() do
    receive do
      message ->
	refute match?({:reply, @server_final}, message)
    after
      @assert_receive_timeout ->
	flunk("Message timed out...")
    end
  end

  defp assert_server_final_timeout() do
    receive do
      _message ->
	flunk("Message should have timed out...")
    after
      @assert_receive_timeout ->
	:ok
    end
  end

  defp assert_server_final_fail_or_timeout() do
    receive do
      message ->
	refute match?({:reply, @server_final}, message)
    after
      @assert_receive_timeout ->
	:ok
    end
  end

end
