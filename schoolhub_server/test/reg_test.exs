defmodule RegTest do

  use ExUnit.Case
  doctest Schoolhub.RegServer

  @test_old_user "test_user"
  @test_old_pw "test_pw"
  @test_new_user "new_user"
  @test_new_pw "new_pw"
  @test_admin "admin"

  
  test "register new user succeeds" do
    result = Schoolhub.RegServer.register_user(@test_new_user, @test_new_pw)
    assert result == :ok
  end

  test "register old user fails" do
    result = Schoolhub.RegServer.register_user(@test_old_user, @test_old_pw)
    assert result == {:error, :user_exists}
  end

  test "register admin fails" do
    result = Schoolhub.RegServer.register_user(@test_admin, @test_new_pw)
    assert result == {:error, :user_exists}
  end
  
end
