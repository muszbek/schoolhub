defmodule CourseGradingTest do

  use ExUnit.Case
  doctest Schoolhub.CourseGradingServer

  @test_user_teacher 'test_user_teacher'
  @test_user_student 'test_user_student'
  @test_user_student2 'test_user_student2'
  @test_user_wrong 'test_user_wrong'
  @test_pw 'test_pw'
  @admin 'admin'
  @test_course 'test_course'
  @test_course_wrong 'test_course_wrong'
  @test_grade %{"grade" => 10}
  @test_grade_append_base %{"old_grade" => 8, "grade" => 9}
  @test_grade_append %{"grade" => 10, "total" => 18}
  @test_grade_append_match %{"old_grade" => 8, "grade" => 10, "total" => 18}

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


  ### SINGULAR GRADING ###

  test "teacher set grade succeeds" do
    :ok = Schoolhub.CourseGradingServer.set_grades(@test_user_teacher, @test_course,
      @test_user_student, @test_grade)
    result = Schoolhub.CourseGradingServer.get_grades(@test_user_teacher, @test_course,
      @test_user_student)
    assert result == @test_grade
  end

  test "admin set grade succeeds" do
    :ok = Schoolhub.CourseGradingServer.set_grades(@admin, @test_course,
      @test_user_student, @test_grade)
    result = Schoolhub.CourseGradingServer.get_grades(@admin, @test_course,
      @test_user_student)
    assert result == @test_grade
  end

  test "student set grade fails" do
    result = Schoolhub.CourseGradingServer.set_grades(@test_user_student, @test_course,
      @test_user_student, @test_grade)
    assert result == {:error, :no_permission}
  end

  test "student get own grade succeeds" do
    result = Schoolhub.CourseGradingServer.get_grades(@test_user_student, @test_course,
      @test_user_student)
    assert result == @test_grade
  end

  test "student get other student grade fails" do
    result = Schoolhub.CourseGradingServer.get_grades(@test_user_student2, @test_course,
      @test_user_student)
    assert result == {:error, :no_permission}
  end

  test "wrong course set grade fails" do
    result = Schoolhub.CourseGradingServer.set_grades(@test_user_teacher, @test_course_wrong,
      @test_user_student, @test_grade)
    assert result == {:error, :course_not_exist}
  end

  test "wrong course get grade fails" do
    result = Schoolhub.CourseGradingServer.get_grades(@test_user_teacher, @test_course_wrong,
      @test_user_student)
    assert result == {:error, :course_not_exist}
  end

  test "wrong target student set grade fails" do
    result = Schoolhub.CourseGradingServer.set_grades(@test_user_teacher, @test_course,
      @test_user_wrong, @test_grade)
    assert result == {:error, :no_affiliation}
  end

  test "wrong target student get grade fails" do
    result = Schoolhub.CourseGradingServer.get_grades(@test_user_teacher, @test_course,
      @test_user_wrong)
    assert result == {:error, :no_affiliation}
  end


  test "teacher append grade succeeds" do
    :ok = Schoolhub.CourseGradingServer.set_grades(@test_user_teacher, @test_course,
      @test_user_student2, @test_grade_append_base)
    :ok = Schoolhub.CourseGradingServer.append_grades(@test_user_teacher, @test_course,
      @test_user_student2, @test_grade_append)
    result = Schoolhub.CourseGradingServer.get_grades(@test_user_teacher, @test_course,
      @test_user_student2)
    assert result == @test_grade_append_match
  end

  test "admin append grade succeeds" do
    :ok = Schoolhub.CourseGradingServer.set_grades(@admin, @test_course,
      @test_user_student2, @test_grade_append_base)
    :ok = Schoolhub.CourseGradingServer.append_grades(@admin, @test_course,
      @test_user_student2, @test_grade_append)
    result = Schoolhub.CourseGradingServer.get_grades(@admin, @test_course,
      @test_user_student2)
    assert result == @test_grade_append_match
  end

  test "student append grade fails" do
    :ok = Schoolhub.CourseGradingServer.set_grades(@test_user_teacher, @test_course,
      @test_user_student2, @test_grade_append_base)
    result = Schoolhub.CourseGradingServer.append_grades(@test_user_student, @test_course,
      @test_user_student2, @test_grade_append)
    assert result == {:error, :no_permission}
  end

  test "wrong course append grade fails" do
    :ok = Schoolhub.CourseGradingServer.set_grades(@test_user_teacher, @test_course,
      @test_user_student2, @test_grade_append_base)
    result = Schoolhub.CourseGradingServer.append_grades(@test_user_teacher, @test_course_wrong,
      @test_user_student2, @test_grade_append)
    assert result == {:error, :course_not_exist}
  end

  test "wrong target student append grade fails" do
    :ok = Schoolhub.CourseGradingServer.set_grades(@test_user_teacher, @test_course,
      @test_user_student2, @test_grade_append_base)
    result = Schoolhub.CourseGradingServer.append_grades(@test_user_teacher, @test_course,
      @test_user_wrong, @test_grade_append)
    assert result == {:error, :no_affiliation}
  end


  ### MASS GRADING ###

  test "teacher mass set grade succeeds" do
    grade_list = [{@test_user_student, @test_grade}, {@test_user_student2, @test_grade}]
    :ok = Schoolhub.CourseGradingServer.mass_set_grades(@test_user_teacher, @test_course,
      grade_list)
    result = Schoolhub.CourseGradingServer.get_grades(@test_user_teacher, @test_course,
      @test_user_student)
    assert result == @test_grade
  end

  test "admin mass set grade succeeds" do
    grade_list = [{@test_user_student, @test_grade}, {@test_user_student2, @test_grade}] 
    :ok = Schoolhub.CourseGradingServer.mass_set_grades(@admin, @test_course, grade_list)
    result = Schoolhub.CourseGradingServer.get_grades(@admin, @test_course, @test_user_student)
    assert result == @test_grade
  end

  test "student mass set grade fails" do
    grade_list = [{@test_user_student, @test_grade}, {@test_user_student2, @test_grade}] 
    result = Schoolhub.CourseGradingServer.mass_set_grades(@test_user_student, @test_course,
      grade_list)
    assert result == {:error, :no_permission}
  end

  test "wrong course mass set grade fails" do
    grade_list = [{@test_user_student, @test_grade}, {@test_user_student2, @test_grade}] 
    result = Schoolhub.CourseGradingServer.mass_set_grades(@test_user_teacher, @test_course_wrong,
      grade_list)
    assert result == {:error, :course_not_exist}
  end

  test "containing wrong student mass set grade succeeds" do
    ## Some might still succeed, if the list is wrong we don't care.
    grade_list = [{@test_user_student, @test_grade}, {@test_user_wrong, @test_grade}] 
    result = Schoolhub.CourseGradingServer.mass_set_grades(@test_user_teacher, @test_course,
      grade_list)
    assert result == :ok
  end


  test "teacher mass append grade succeeds" do
    grade_list = [{@test_user_student, @test_grade}, {@test_user_student2, @test_grade}]
    :ok = Schoolhub.CourseGradingServer.mass_append_grades(@test_user_teacher, @test_course,
      grade_list)
    result = Schoolhub.CourseGradingServer.get_grades(@test_user_teacher, @test_course,
      @test_user_student)
    ## Get student2 just to fire off the latching return in the mock database
    _anything = Schoolhub.CourseGradingServer.get_grades(@test_user_teacher, @test_course,
      @test_user_student2)
    assert result == @test_grade
  end

  test "admin mass append grade succeeds" do
    grade_list = [{@test_user_student, @test_grade}, {@test_user_student2, @test_grade}]
    :ok = Schoolhub.CourseGradingServer.mass_append_grades(@admin, @test_course, grade_list)
    result = Schoolhub.CourseGradingServer.get_grades(@admin, @test_course, @test_user_student)
    ## Get student2 just to fire off the latching return in the mock database
    _anything = Schoolhub.CourseGradingServer.get_grades(@admin, @test_course, @test_user_student2)
    assert result == @test_grade
  end

  test "student mass append grade fails" do
    grade_list = [{@test_user_student, @test_grade}, {@test_user_student2, @test_grade}]
    result = Schoolhub.CourseGradingServer.mass_append_grades(@test_user_student, @test_course,
      grade_list)
    assert result == {:error, :no_permission}
  end

  test "mass append grade with key succeeds" do
    grade_list = [{@test_user_student, 10}, {@test_user_student2, 10}]
    :ok = Schoolhub.CourseGradingServer.mass_append_grades(@test_user_teacher, @test_course,
      grade_list, "grade")
    result = Schoolhub.CourseGradingServer.get_grades(@test_user_teacher, @test_course,
      @test_user_student)
    ## Get student2 just to fire off the latching return in the mock database
    _anything = Schoolhub.CourseGradingServer.get_grades(@test_user_teacher, @test_course,
      @test_user_student2)
    assert result == @test_grade
  end

  test "wrong course mass append grade fails" do
    grade_list = [{@test_user_student, @test_grade}, {@test_user_student2, @test_grade}]
    result = Schoolhub.CourseGradingServer.mass_append_grades(@test_user_teacher,
      @test_course_wrong, grade_list)
    assert result == {:error, :course_not_exist}
  end

  test "containing wrong student mass append grade succeeds" do
    grade_list = [{@test_user_student, @test_grade}, {@test_user_wrong, @test_grade}]
    result = Schoolhub.CourseGradingServer.mass_append_grades(@test_user_teacher, @test_course,
      grade_list)
    assert result == :ok
  end
  
end
