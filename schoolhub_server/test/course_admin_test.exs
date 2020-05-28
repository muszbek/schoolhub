defmodule CourseAdminTest do

  use ExUnit.Case
  doctest Schoolhub.CourseAdminServer

  @test_user_teacher 'test_user_teacher'
  @test_user_student 'test_user_new'
  @test_pw 'test_pw'
  @admin 'admin'
  @test_course 'test_course'
  @test_course_wrong 'test_course_wrong'

  setup_all do
    :ok = Schoolhub.RegServer.register_user(@test_user_teacher, @test_pw)
    :ok = Schoolhub.RegServer.set_user_privilege(@admin, @test_user_teacher, "teacher")
    on_exit(fn() -> teardown() end)
    :ok
  end

  def teardown() do
    Schoolhub.CourseAdminServer.remove_course(@admin, @test_course)
    Schoolhub.RegServer.remove_user(@test_user_teacher)
    :ok
  end

  
  test "teacher create course succeeds" do
    result = Schoolhub.CourseAdminServer.create_course(@test_user_teacher, @test_course)
    assert result == :ok
  end

  test "owner get affiliation succeeds" do
    :ok = Schoolhub.CourseAdminServer.create_course(@test_user_teacher, @test_course)
    result = Schoolhub.CourseAdminServer.get_affiliation(@test_user_teacher, @test_course)
    assert result == "owner"
  end

  test "not affiliated user no affiliation" do
    :ok = Schoolhub.RegServer.register_user(@test_user_student, @test_pw)
    :ok = Schoolhub.CourseAdminServer.create_course(@test_user_teacher, @test_course)
    result = Schoolhub.CourseAdminServer.get_affiliation(@test_user_student, @test_course)
    assert result == {:error, :no_affiliation}
  end

  test "affiliation on wrong course fails" do
    result = Schoolhub.CourseAdminServer.get_affiliation(@test_user_teacher, @test_course_wrong)
    assert result == {:error, :course_not_exist}
  end

  test "owner remove course succeeds" do
    :ok = Schoolhub.CourseAdminServer.create_course(@test_user_teacher, @test_course)
    result = Schoolhub.CourseAdminServer.remove_course(@test_user_teacher, @test_course)
    assert result == :ok
  end

  test "admin remove course succeeds" do
    :ok = Schoolhub.CourseAdminServer.create_course(@test_user_teacher, @test_course)
    result = Schoolhub.CourseAdminServer.remove_course(@admin, @test_course)
    assert result == :ok
  end

  test "student remove course fails" do
    :ok = Schoolhub.RegServer.register_user(@test_user_student, @test_pw)
    :ok = Schoolhub.CourseAdminServer.create_course(@test_user_teacher, @test_course)
    result = Schoolhub.CourseAdminServer.remove_course(@test_user_student, @test_course)
    assert result == {:error, :no_permission}
  end
  
end
