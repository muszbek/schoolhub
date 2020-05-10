defmodule CourseMembershipTest do

  use ExUnit.Case
  doctest Schoolhub.CourseServer

  @test_user_teacher 'test_user_teacher'
  @test_user_student 'test_user_student'
  @test_user_wrong 'test_user_wrong'
  @test_pw 'test_pw'
  @admin 'admin'
  @test_course 'test_course'
  @test_course_wrong 'test_course_wrong'
  
  setup_all do
    :ok = Schoolhub.RegServer.register_user(@test_user_teacher, @test_pw)
    :ok = Schoolhub.RegServer.register_user(@test_user_student, @test_pw)
    :ok = Schoolhub.RegServer.set_user_privilege(@admin, @test_user_teacher, "teacher")
    :ok = Schoolhub.CourseServer.create_course(@test_user_teacher, @test_course)
    on_exit(fn() -> teardown() end)
    :ok
  end

  setup do
    Schoolhub.CourseServer.remove_course(@admin, @test_course)
    :ok = Schoolhub.CourseServer.create_course(@test_user_teacher, @test_course)
  end

  def teardown() do
    Schoolhub.CourseServer.remove_course(@admin, @test_course)
    Schoolhub.RegServer.remove_user(@test_user_student)
    Schoolhub.RegServer.remove_user(@test_user_teacher)
    :ok
  end

  
  test "teacher invite student succeeds" do
    result = Schoolhub.CourseServer.invite_student(@test_user_teacher,
      @test_user_student, @test_course)
    assert result == :ok
  end

  test "admin invite student succeeds" do
    result = Schoolhub.CourseServer.invite_student(@admin,
      @test_user_student, @test_course)
    assert result == :ok
  end

  test "invite to wrong course fails" do
    result = Schoolhub.CourseServer.invite_student(@test_user_teacher,
      @test_user_student, @test_course_wrong)
    assert result == {:error, :course_not_exist}
  end

  test "invite wrong user fails" do
    result = Schoolhub.CourseServer.invite_student(@test_user_teacher,
      @test_user_wrong, @test_course)
    assert result == {:error, :user_not_exist}
  end

  test "student invite user fails" do
    result = Schoolhub.CourseServer.invite_student(@test_user_student,
      @test_user_student, @test_course)
    assert result == {:error, :no_permission}
  end
  
end
