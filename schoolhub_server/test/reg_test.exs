defmodule RegTest do

  use ExUnit.Case
  doctest Schoolhub.RegServer

  @test_old_user "test_user"
  @test_old_pw "test_pw"
  @test_new_user "new_user"
  @test_new_pw "new_pw"
  @test_regger "reg_agent"
  @test_replacable_user "replacable_user"

  
  test "register new user succeeds" do
    result = Schoolhub.RegServer.register_user(@test_new_user, @test_new_pw)
    assert result == :ok
  end

  test "register old user fails" do
    result = Schoolhub.RegServer.register_user(@test_old_user, @test_old_pw)
    assert result == {:error, :user_exists}
  end

  test "register regger fails" do
    result = Schoolhub.RegServer.register_user(@test_regger, @test_new_pw)
    assert result == {:error, :user_exists}
  end


  
  test "remove existing user succeeds" do
    result = Schoolhub.RegServer.remove_user(@test_old_user)
    assert result == {:ok, :user_removed}
  end

  test "remove non existing user succeeds" do
    result = Schoolhub.RegServer.remove_user(@test_new_user)
    assert result == {:ok, :user_not_existed}
  end


  
  test "change existing user password succeeds" do
    result = Schoolhub.RegServer.change_user_pw(@test_replacable_user, @test_new_pw)
    assert result == :ok
  end

  test "change non existing user password fails" do
    result = Schoolhub.RegServer.change_user_pw(@test_new_user, @test_old_pw)
    assert result == {:error, :user_not_exist}
  end

  test "change user with wrong password fails" do
    result = Schoolhub.RegServer.change_user_pw(@test_old_user, "")
    assert result == {:error, :password_invalid}
  end
  
end
