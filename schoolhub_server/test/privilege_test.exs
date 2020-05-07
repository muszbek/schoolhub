defmodule PrivilegeTest do

  use ExUnit.Case
  doctest Schoolhub.RegServer

  @test_user 'test_user_new'
  @test_pw 'test_pw'
  @admin 'admin'
  @test_user_wrong 'test_user_wrong'

  setup_all do
    :ok = Schoolhub.RegServer.register_user(@test_user, @test_pw)
    on_exit(fn() -> teardown() end)
    :ok
  end

  def teardown() do
    Schoolhub.RegServer.remove_user(@test_user)
    :ok
  end

  
  test "non admin change privilege fails" do
    result = Schoolhub.RegServer.set_user_privilege(@test_user, @admin, "admin")
    assert result == {:error, :no_permission}
  end

  test "admin change privilege succeeds" do
    result = Schoolhub.RegServer.set_user_privilege(@admin, @test_user, "teacher")
    assert result == :ok
  end

  test "wrong privilege fails" do
    result = Schoolhub.RegServer.set_user_privilege(@admin, @test_user, "unknown")
    assert result == {:error, :worng_privilege}
  end

  test "non existing user change privilege fails" do
    result = Schoolhub.RegServer.set_user_privilege(@admin, @test_user_wrong, "teacher")
    assert result == {:error, :worng_privilege}
  end

  test "self change privilege fails" do
    result = Schoolhub.RegServer.set_user_privilege(@test_user, @test_user, "teacher")
    assert result == {:error, :set_self_privilege}
  end
  
end
