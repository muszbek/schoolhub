defmodule CourseContentTest do

  use ExUnit.Case
  doctest Schoolhub.CourseContentServer

  @test_user_teacher 'test_user_teacher'
  @test_user_student 'test_user_student'
  @test_pw 'test_pw'
  @admin 'admin'
  @test_course 'test_course'
  @test_course_wrong 'test_course_wrong'
  @test_desc %{"text" => "test"}
  @test_desc_short %{text: "test"}
  @test_desc_string "{\"text\":\"test\"}"
  @test_desc_charlist '{\"text\":\"test\"}'

  setup_all do
    :ok = Schoolhub.RegServer.register_user(@test_user_teacher, @test_pw)
    :ok = Schoolhub.RegServer.register_user(@test_user_student, @test_pw)
    :ok = Schoolhub.RegServer.set_user_privilege(@admin, @test_user_teacher, "teacher")
    :ok = Schoolhub.CourseAdminServer.create_course(@test_user_teacher, @test_course)
    on_exit(fn() -> teardown() end)
    :ok
  end

  setup do
    Schoolhub.CourseAdminServer.remove_course(@admin, @test_course)
    :ok = Schoolhub.CourseAdminServer.create_course(@test_user_teacher, @test_course)
  end

  def teardown() do
    Schoolhub.CourseAdminServer.remove_course(@admin, @test_course)
    Schoolhub.RegServer.remove_user(@test_user_student)
    Schoolhub.RegServer.remove_user(@test_user_teacher)
    :ok
  end


  test "teacher set desc as map succeeds" do
    :ok = Schoolhub.CourseContentServer.set_description(@test_user_teacher,
      @test_course, @test_desc)
    result = Schoolhub.CourseContentServer.get_description(@test_course)
    assert result == @test_desc
  end

  test "teacher set desc as short syntax map succeeds" do
    :ok = Schoolhub.CourseContentServer.set_description(@test_user_teacher,
      @test_course, @test_desc_short)
    result = Schoolhub.CourseContentServer.get_description(@test_course)
    assert result == @test_desc
  end

  test "teacher set desc as string succeeds" do
    :ok = Schoolhub.CourseContentServer.set_description(@test_user_teacher,
      @test_course, @test_desc_string)
    result = Schoolhub.CourseContentServer.get_description(@test_course)
    assert result == @test_desc
  end

  test "teacher set desc as charlist succeeds" do
    :ok = Schoolhub.CourseContentServer.set_description(@test_user_teacher,
      @test_course, @test_desc_charlist)
    result = Schoolhub.CourseContentServer.get_description(@test_course)
    assert result == @test_desc
  end

  test "admin set desc succeeds" do
    :ok = Schoolhub.CourseContentServer.set_description(@admin,
      @test_course, @test_desc)
    result = Schoolhub.CourseContentServer.get_description(@test_course)
    assert result == @test_desc
  end

  test "student set desc fails" do
    result = Schoolhub.CourseContentServer.set_description(@test_user_student,
      @test_course, @test_desc)
    assert result == {:error, :no_permission}
  end

  test "set desc on wrong course fails" do
    result = Schoolhub.CourseContentServer.set_description(@test_user_teacher,
      @test_course_wrong, @test_desc)
    assert result == {:error, :course_not_exist}
  end

end
