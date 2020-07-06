defmodule Schoolhub.DataManagerMock do
  @moduledoc """
  API for accessing a mock database with mock data.
  Use for testing.
  """

  @mock_user 'test_user'
  @mock_user_string "test_user"
  @mock_user_new 'test_user_new'
  @mock_user_new_string "test_user_new"
  @mock_replacable_user 'replacable_user'
  @mock_replacable_user_string "replacable_user"
  #@mock_archive_message "The quick brown fox jumps over the lazy dog."
  @mock_regger 'reg_agent'
  @mock_regger_string "reg_agent"
  @mock_admin 'admin'
  @mock_admin_string "admin"
  @mock_user_wrong 'test_user_wrong'
  @mock_user_wrong_string "test_user_wrong"
  @mock_user_teacher 'test_user_teacher'
  @mock_user_teacher_string "test_user_teacher"
  @mock_user_student 'test_user_student'
  @mock_user_student_string "test_user_student"
  @mock_user_student2 'test_user_student2'
  @mock_user_student2_string "test_user_student2"
  @mock_course 'test_course'
  @mock_course_string "test_course"
  @mock_course_desc %{"text" => "test"}
  @mock_course_desc_short %{text: "test"}

  @scram_prefix "==SCRAM==,"
  @mock_scram "==SCRAM==,jv1SCgihx+Q2yj6PggxUZPbmfp4=,r+T1xjRnDwpUPoC/EwOXA+Jjt2Y=,iCgKQkjMSgfZgjh06UMZzg==,4096"
  ## The password belonging to this mock entry is "test_pw"
  @admin_scram "==SCRAM==,q8IV+Dmd+cGd4/0HeuXIpcQKfbY=,pQcMX+3lNyu4XQHOAPqI7RkVMOg=,3xUQoBdvCUdUQCWJYCYAIg==,4096"
  ## The password belonging to this mock entry is "admin"

  ### API functions ###
  
  def start_link([]) do
    :ok
  end

  
  def get_scram_pw(@mock_user) do
    @mock_scram
  end
  def get_scram_pw(@mock_user_string) do
    get_scram_pw(@mock_user)
  end
  def get_scram_pw(@mock_user_new) do
    @mock_scram
  end
  def get_scram_pw(@mock_user_new_string) do
    get_scram_pw(@mock_user_new)
  end
  def get_scram_pw(@mock_user_teacher) do
    @mock_scram
  end
  def get_scram_pw(@mock_user_teacher_string) do
    get_scram_pw(@mock_user_teacher)
  end
  def get_scram_pw(@mock_user_student) do
    @mock_scram
  end
  def get_scram_pw(@mock_user_student_string) do
    get_scram_pw(@mock_user_student)
  end
  def get_scram_pw(@mock_user_student2) do
    @mock_scram
  end
  def get_scram_pw(@mock_user_student2_string) do
    get_scram_pw(@mock_user_student2)
  end
  def get_scram_pw(@mock_admin) do
    @admin_scram
  end
  def get_scram_pw(@mock_admin_string) do
    get_scram_pw(@mock_admin)
  end
  def get_scram_pw(_other_user) do
    :nil
  end

  def check_user_exist(@mock_user) do
    true
  end
  def check_user_exist(@mock_user_string) do
    check_user_exist(@mock_user)
  end
  def check_user_exist(@mock_regger) do
    true
  end
  def check_user_exist(@mock_regger_string) do
    check_user_exist(@mock_regger)
  end
  def check_user_exist(@mock_replacable_user) do
    case flag() do
      :set -> true
      :reset -> false
    end
  end
  def check_user_exist(@mock_replacable_user_string) do
    check_user_exist(@mock_replacable_user)
  end
  def check_user_exist(_other_user) do
    false
  end

  def add_scram_user(@mock_user, @scram_prefix <> _rest_of_scram) do
    :user_exists
  end
  def add_scram_user(@mock_user_string, scram) do
    add_scram_user(@mock_user, scram)
  end
  def add_scram_user(_other_user, @scram_prefix <> _rest_of_scram) do
    :ok
  end

  def remove_scram_user(@mock_user) do
    {:ok, :user_removed}
  end
  def remove_scram_user(@mock_user_string) do
    remove_scram_user(@mock_user)
  end
  def remove_scram_user(@mock_replacable_user) do
    {:ok, :user_removed}
  end
  def remove_scram_user(@mock_replacable_user_string) do
    remove_scram_user(@mock_replacable_user)
  end
  def remove_scram_user(_other_user) do
    {:ok, :user_not_existed}
  end

  def purge_user(user) do
    remove_scram_user(user)
  end

  def get_archive(_, _, _) do
    []
  end

  def add_user_privilege(_username) do
    :ok
  end

  def get_user_privilege(@mock_admin) do
    "admin"
  end
  def get_user_privilege(@mock_admin_string) do
    get_user_privilege(@mock_admin)
  end
  def get_user_privilege(@mock_user_wrong) do
    :nil
  end
  def get_user_privilege(@mock_user_wrong_string) do
    get_user_privilege(@mock_user_wrong)
  end
  def get_user_privilege(@mock_user_teacher) do
    "teacher"
  end
  def get_user_privilege(@mock_user_teacher_string) do
    get_user_privilege(@mock_user_teacher)
  end
  def get_user_privilege(_username) do
    "student"
  end

  def get_all_privilege() do
    [[@mock_admin_string, "admin"], [@mock_user_new_string, "student"]]
  end

  def set_user_privilege(@mock_admin, 'admin') do
    :ok
  end
  def set_user_privilege(@mock_admin_string, "admin") do
    set_user_privilege(@mock_admin, 'admin')
  end
  def set_user_privilege(@mock_user_new, 'teacher') do
    :ok
  end
  def set_user_privilege(@mock_user_new_string, "teacher") do
    set_user_privilege(@mock_user_new, 'teacher')
  end
  def set_user_privilege(@mock_user_teacher, 'teacher') do
    :ok
  end
  def set_user_privilege(@mock_user_teacher_string, "teacher") do
    set_user_privilege(@mock_user_teacher, 'teacher')
  end

  def create_course(@mock_course, _owner) do
    :ok
  end
  def create_course(@mock_course_string, owner) do
    create_course(@mock_course, owner)
  end

  def remove_course(_course) do
    :ok
  end

  def get_affiliation(@mock_user_teacher, @mock_course) do
    "owner"
  end
  def get_affiliation(@mock_user_teacher_string, @mock_course_string) do
    get_affiliation(@mock_user_teacher, @mock_course)
  end
  def get_affiliation(@mock_user_student, @mock_course) do
    "student"
  end
  def get_affiliation(@mock_user_student_string, @mock_course_string) do
    get_affiliation(@mock_user_student, @mock_course)
  end
  def get_affiliation(@mock_user_student2, @mock_course) do
    "student"
  end
  def get_affiliation(@mock_user_student2_string, @mock_course_string) do
    get_affiliation(@mock_user_student, @mock_course)
  end
  def get_affiliation(_other, @mock_course) do
    {:error, :no_affiliation}
  end
  def get_affiliation(other, @mock_course_string) do
    get_affiliation(other, @mock_course)
  end
  def get_affiliation(_any_user, _other_course) do
    {:error, :course_not_exist}
  end

  def get_all_affiliation(@mock_course) do
    [[@mock_user_teacher_string, "owner"], [@mock_user_student_string, "student"]]
  end
  def get_all_affiliation(@mock_course_string) do
    get_all_affiliation(@mock_course)
  end
  def get_all_affiliation(_other_course) do
    {:error, :course_not_exist}
  end

  def invite_student(@mock_user_student, @mock_course) do
    :ok
  end
  def invite_student(@mock_user_student_string, @mock_course_string) do
    invite_student(@mock_user_student, @mock_course)
  end
  def invite_student(@mock_user_student2, @mock_course) do
    :ok
  end
  def invite_student(@mock_user_student2_string, @mock_course_string) do
    invite_student(@mock_user_student2, @mock_course)
  end
  def invite_student(@mock_user_teacher, @mock_course) do
    {:ok, :already_invited}
  end
  def invite_student(@mock_user_teacher_string, @mock_course_string) do
    invite_student(@mock_user_teacher, @mock_course)
  end
  def invite_student(@mock_user_student, _other_course) do
    {:error, :course_not_exist}
  end
  def invite_student(@mock_user_student_string, other_course) do
    invite_student(@mock_user_student, other_course)
  end
  def invite_student(_other_student, _any_course) do
    {:error, :user_not_exist}
  end

  def remove_student(@mock_user_student, @mock_course) do
    :ok
  end
  def remove_student(@mock_user_student_string, @mock_course_string) do
    remove_student(@mock_user_student, @mock_course)
  end
  def remove_student(_other_student, @mock_course) do
    {:ok, :already_removed}
  end
  def remove_student(other_student, @mock_course_string) do
    remove_student(other_student, @mock_course)
  end
  def remove_student(_any_student, _other_course) do
    {:error, :course_not_exist}
  end

  def set_affiliation(@mock_user_student, @mock_course, "assistant") do
    :ok
  end
  def set_affiliation(@mock_user_student_string, @mock_course_string, aff) do
    set_affiliation(@mock_user_student, @mock_course, aff)
  end
  def set_affiliation(_other_user, _any_course, _any_aff) do
    {:error, :user_not_affiliated}
  end

  def get_course_desc(@mock_course) do
    @mock_course_desc
  end
  def get_course_desc(@mock_course_string) do
    get_course_desc(@mock_course)
  end
  def get_course_desc(_other_course) do
    {:error, :course_not_exist}
  end

  def set_course_desc(@mock_course, @mock_course_desc) do
    :ok
  end
  def set_course_desc(@mock_course_string, @mock_course_desc) do
    set_course_desc(@mock_course, @mock_course_desc)
  end
  def set_course_desc(@mock_course, @mock_course_desc_short) do
    :ok
  end
  def set_course_desc(@mock_course_string, @mock_course_desc_short) do
    set_course_desc(@mock_course, @mock_course_desc_short)
  end
  def set_course_desc(_other_course, _any_course_desc) do
    {:error, :course_not_exist}
  end

  def get_grades(@mock_course, @mock_user_student) do
    %{"grade" => 10}
  end
  def get_grades(@mock_course_string, @mock_user_student_string) do
    get_grades(@mock_course, @mock_user_student)
  end
  def get_grades(@mock_course, @mock_user_student2) do
    case flag() do
      :set -> %{"old_grade" => 8, "grade" => 9}
      :reset -> %{"old_grade" => 8, "grade" => 10, "total" => 18}
    end
  end
  def get_grades(@mock_course_string, @mock_user_student2_string) do
    get_grades(@mock_course, @mock_user_student2)
  end
  def get_grades(@mock_course, _other_student) do
    {:error, :no_affiliation}
  end
  def get_grades(@mock_course_string, other_student) do
    get_grades(@mock_course, other_student)
  end

  def set_grades(@mock_course, @mock_user_student, _any_grades) do
    :ok
  end
  def set_grades(@mock_course_string, @mock_user_student_string, any_grades) do
    set_grades(@mock_course, @mock_user_student, any_grades)
  end
  def set_grades(@mock_course, @mock_user_student2, _any_grades) do
    :ok
  end
  def set_grades(@mock_course_string, @mock_user_student2_string, any_grades) do
    set_grades(@mock_course, @mock_user_student, any_grades)
  end
  def set_grades(@mock_course, _other_student, _any_grades) do
    {:error, :no_affiliation}
  end
  def set_grades(@mock_course_string, other_student, any_grades) do
    set_grades(@mock_course, other_student, any_grades)
  end
    
    
  defp flag() do
    spawn_flag = fn() ->
      receive do
	:kill -> :ok
      end
    end

    if Process.whereis(:flag) == :nil do
      Process.register(Process.spawn(spawn_flag,[]), :flag)
      :set
    else
      Process.send(:flag, :kill, [])
      :reset
    end
  end
  
end
