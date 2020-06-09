defmodule CourseContentTest do

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
  @test_desc_short %{text: "test"}
  @test_desc_json_string "{\"text\":\"test\"}"
  @test_desc_json_charlist '{\"text\":\"test\"}'

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


  ### COURSE DESCRIPTION ###
  
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

  test "teacher set desc as json string succeeds" do
    :ok = Schoolhub.CourseContentServer.set_description(@test_user_teacher,
      @test_course, @test_desc_json_string)
    result = Schoolhub.CourseContentServer.get_description(@test_course)
    assert result == @test_desc
  end

  test "teacher set desc as json charlist succeeds" do
    :ok = Schoolhub.CourseContentServer.set_description(@test_user_teacher,
      @test_course, @test_desc_json_charlist)
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


  ### COURSE MESSAGE BASIC ###
  
  test "student post message succeeds" do
    {:ok, id} = Schoolhub.CourseContentServer.post_message(@test_user_student,
      @test_course, @test_desc)
    result = Schoolhub.CourseContentServer.get_single_message(id, @test_user_student,
      @test_course)
    assert %{message: @test_desc} = result
  end

  test "not affiliated post message fails" do
    result = Schoolhub.CourseContentServer.post_message(@test_user_wrong,
      @test_course, @test_desc)
    assert result == {:error, :no_affiliation}
  end

  test "student post message wrong course fails" do
    result = Schoolhub.CourseContentServer.post_message(@test_user_student,
      @test_course_wrong, @test_desc)
    assert result == {:error, :course_not_exist}
  end

  test "not affiliated get single message fails" do
    result = Schoolhub.CourseContentServer.get_single_message(1, @test_user_wrong,
      @test_course)
    assert result == {:error, :no_affiliation}
  end

  test "student get single message wrong course fails" do
    result = Schoolhub.CourseContentServer.get_single_message(1, @test_user_student,
      @test_course_wrong)
    assert result == {:error, :course_not_exist}
  end

  test "student get single message wrong id fails" do
    result = Schoolhub.CourseContentServer.get_single_message(0, @test_user_student,
      @test_course)
    assert result == {:error, :message_not_exist}
  end


  test "student post reply succeeds" do
    {:ok, id} = Schoolhub.CourseContentServer.post_reply(1, @test_user_student,
      @test_course, @test_desc)
    result = Schoolhub.CourseContentServer.get_single_message(id, @test_user_student,
      @test_course)
    assert %{message: @test_desc} = result
  end

  test "not affiliated post reply fails" do
    result = Schoolhub.CourseContentServer.post_reply(1, @test_user_wrong,
      @test_course, @test_desc)
    assert result == {:error, :no_affiliation}
  end

  test "student post reply wrong course fails" do
    result = Schoolhub.CourseContentServer.post_reply(1, @test_user_student,
      @test_course_wrong, @test_desc)
    assert result == {:error, :course_not_exist}
  end

  test "student post reply wrong id fails" do
    result = Schoolhub.CourseContentServer.post_reply(0, @test_user_student,
      @test_course, @test_desc)
    assert result == {:error, :origin_not_exist}
  end


  test "teacher delete single message succeeds" do
    {:ok, id} = Schoolhub.CourseContentServer.post_message(@test_user_student,
      @test_course, @test_desc)
    result = Schoolhub.CourseContentServer.delete_single_message(id, @test_user_teacher,
      @test_course)
    assert result == :ok
  end

  test "teacher delete deleted single message succeeds" do
    result = Schoolhub.CourseContentServer.delete_single_message(1, @test_user_teacher,
      @test_course)
    assert result == :ok
  end

  test "admin delete single message succeeds" do
    {:ok, id} = Schoolhub.CourseContentServer.post_message(@test_user_student,
      @test_course, @test_desc)
    result = Schoolhub.CourseContentServer.delete_single_message(id, @admin,
      @test_course)
    assert result == :ok
  end

  test "student author delete single message succeeds" do
    {:ok, id} = Schoolhub.CourseContentServer.post_message(@test_user_student,
      @test_course, @test_desc)
    result = Schoolhub.CourseContentServer.delete_single_message(id, @test_user_student,
      @test_course)
    assert result == :ok
  end

  test "student non author delete single message fails" do
    {:ok, id} = Schoolhub.CourseContentServer.post_message(@test_user_student,
      @test_course, @test_desc)
    result = Schoolhub.CourseContentServer.delete_single_message(id, @test_user_student2,
      @test_course)
    assert result == {:error, :no_permission}
  end

  test "not affiliated delete single message fails" do
    {:ok, id} = Schoolhub.CourseContentServer.post_message(@test_user_student,
      @test_course, @test_desc)
    result = Schoolhub.CourseContentServer.delete_single_message(id, @test_user_wrong,
      @test_course)
    assert result == {:error, :no_affiliation}
  end

  test "wrong course delete single message fails" do
    {:ok, id} = Schoolhub.CourseContentServer.post_message(@test_user_student,
      @test_course, @test_desc)
    result = Schoolhub.CourseContentServer.delete_single_message(id, @test_user_teacher,
      @test_course_wrong)
    assert result == {:error, :course_not_exist}
  end

end
