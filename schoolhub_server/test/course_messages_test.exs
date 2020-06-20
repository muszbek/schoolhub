defmodule CourseMessagesTest do

  use ExUnit.Case
  doctest Schoolhub.CourseContentServer

  @test_user_teacher 'test_user_teacher'
  @test_user_student 'test_user_student'
  @test_user_student2 'test_user_student2'
  @test_user_wrong 'test_user_wrong'
  @test_pw 'test_pw'
  @admin 'admin'
  @test_course 'test_course'
  @test_course_wrong 'test_course_wrong'
  @test_desc %{"text" => "test"}

  setup_all do
    :ok = Schoolhub.RegServer.register_user(@test_user_teacher, @test_pw)
    :ok = Schoolhub.RegServer.register_user(@test_user_student, @test_pw)
    :ok = Schoolhub.RegServer.register_user(@test_user_student2, @test_pw)
    :ok = Schoolhub.RegServer.set_user_privilege(@admin, @test_user_teacher, "teacher")
    :ok = Schoolhub.CourseAdminServer.create_course(@test_user_teacher, @test_course)
    :ok = Schoolhub.CourseAdminServer.invite_student(@test_user_teacher,
      @test_user_student, @test_course)
    :ok = Schoolhub.CourseAdminServer.invite_student(@test_user_teacher,
      @test_user_student2, @test_course)
    {:ok, id} = Schoolhub.CourseContentServer.post_message(@test_user_student,
      @test_course, @test_desc)
    {:ok, _id} = Schoolhub.CourseContentServer.post_reply(id, @test_user_student,
      @test_course, @test_desc)
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
    Schoolhub.RegServer.remove_user(@test_user_student2)
    Schoolhub.RegServer.remove_user(@test_user_teacher)
    :ok
  end


  test "student get root messages succeeds" do
    result = Schoolhub.CourseContentServer.get_root_messages(@test_user_student, @test_course)
    assert [%Schoolhub.Post{replies: 1}] = result
  end

  test "not affiliated get root messages fails" do
    result = Schoolhub.CourseContentServer.get_root_messages(@test_user_wrong, @test_course)
    assert result == {:error, :no_affiliation}
  end

  test "student get root messages wrong course fails" do
    result = Schoolhub.CourseContentServer.get_root_messages(@test_user_student,
      @test_course_wrong)
    assert result == {:error, :course_not_exist}
  end


  test "student get replies succeess" do
    result = Schoolhub.CourseContentServer.get_replies(1, @test_user_student, @test_course)
    assert [%Schoolhub.Post{id: 1, ancestor: nil}, %Schoolhub.Post{ancestor: 1}] = result
  end

  test "not affiliated get replies fails" do
    result = Schoolhub.CourseContentServer.get_replies(1, @test_user_wrong, @test_course)
    assert result == {:error, :no_affiliation}
  end

  test "student get replies wrong course fails" do
    result = Schoolhub.CourseContentServer.get_replies(1, @test_user_student,
      @test_course_wrong)
    assert result == {:error, :course_not_exist}
  end

  test "get replies wrong id returns empty" do
    result = Schoolhub.CourseContentServer.get_replies(0, @test_user_student, @test_course)
    assert result == []
  end
  
end
