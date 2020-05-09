defmodule CourseAdminTest do

  use ExUnit.Case
  doctest Schoolhub.CourseServer

  @test_user_teacher 'test_user_teacher'
  @test_user_student 'test_user_new'
  @test_pw 'test_pw'
  @admin 'admin'
  @test_course 'test_course'

  setup_all do
    :ok = Schoolhub.RegServer.register_user(@test_user_teacher, @test_pw)
    on_exit(fn() -> teardown() end)
    :ok
  end

  def teardown() do
    Schoolhub.CourseServer.remove_course(@admin, @test_course)
    Schoolhub.RegServer.remove_user(@test_user_teacher)
    :ok
  end

  
  test "teacher create course succeeds" do
    result = Schoolhub.CourseServer.create_course(@test_user_teacher, @test_course)
    assert result == :ok
  end

end
