defmodule CourseMessagesTest do

  use ExUnit.Case
  doctest Schoolhub.CourseContentServer

  @test_user_teacher 'test_user_teacher'
  @test_user_student 'test_user_student'
  @test_user_wrong 'test_user_wrong'
  @test_pw 'test_pw'
  @admin 'admin'
  @test_course 'test_course'
  @test_course_wrong 'test_course_wrong'
  @test_desc %{"text" => "test"}

  setup_all do
    :ok = Schoolhub.RegServer.register_user(@test_user_teacher, @test_pw)
    :ok = Schoolhub.RegServer.register_user(@test_user_student, @test_pw)
    :ok = Schoolhub.RegServer.set_user_privilege(@admin, @test_user_teacher, "teacher")
    :ok = Schoolhub.CourseAdminServer.create_course(@test_user_teacher, @test_course)
    :ok = Schoolhub.CourseAdminServer.invite_student(@test_user_teacher,
      @test_user_student, @test_course)
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


  test "teacher pin message succeeds" do
    result = Schoolhub.CourseContentServer.pin_message(1, @test_user_teacher, @test_course)
    assert result == :ok
  end

  test "admin pin message succeeds" do
    result = Schoolhub.CourseContentServer.pin_message(1, @admin, @test_course)
    assert result == :ok
  end

  test "student pin message fails" do
    result = Schoolhub.CourseContentServer.pin_message(1, @test_user_student, @test_course)
    assert result == {:error, :no_permission}
  end

  test "pin message wrong course fails" do
    result = Schoolhub.CourseContentServer.pin_message(1, @test_user_teacher, @test_course_wrong)
    assert result == {:error, :course_not_exist}
  end

  test "pin message wrong id fails" do
    result = Schoolhub.CourseContentServer.pin_message(0, @test_user_teacher, @test_course)
    assert result == {:error, :message_not_exist}
  end

  test "pin reply fails" do
    result = Schoolhub.CourseContentServer.get_replies(1, @test_user_teacher, @test_course)
    [%Schoolhub.Post{}, %Schoolhub.Post{id: id}] = result
    
    result = Schoolhub.CourseContentServer.pin_message(id, @test_user_teacher, @test_course)
    assert result == {:error, :message_not_exist}
  end


  test "teacher delete root message succeeds" do
    result = Schoolhub.CourseContentServer.delete_root_message(1, @test_user_teacher, @test_course)
    assert result == :ok
  end

  test "admin delete root message succeeds" do
    result = Schoolhub.CourseContentServer.delete_root_message(1, @admin, @test_course)
    assert result == :ok
  end

  test "student delete root message fails" do
    result = Schoolhub.CourseContentServer.delete_root_message(1, @test_user_student, @test_course)
    assert result == {:error, :no_permission}
  end

  test "delete root message wrong course fails" do
    result = Schoolhub.CourseContentServer.delete_root_message(1, @test_user_teacher,
      @test_course_wrong)
    assert result == {:error, :course_not_exist}
  end

  test "delete non existing root message succeeds" do
    result = Schoolhub.CourseContentServer.delete_root_message(0, @test_user_teacher, @test_course)
    assert result == :ok
  end

  test "delete reply succeeds" do
    # returns ok, but it is not deleted in fact
    result = Schoolhub.CourseContentServer.get_replies(1, @test_user_teacher, @test_course)
    [%Schoolhub.Post{}, %Schoolhub.Post{id: id}] = result
    
    result = Schoolhub.CourseContentServer.delete_root_message(id, @test_user_teacher,
      @test_course)
    assert result == :ok
  end
  
end
