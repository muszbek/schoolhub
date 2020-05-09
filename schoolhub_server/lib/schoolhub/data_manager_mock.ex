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
  @mock_course 'test_course'
  @mock_course_string "test_course"

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
  def get_affiliation(_other, @mock_course) do
    {:error, :no_affiliation}
  end
  def get_affiliation(other, @mock_course_string) do
    get_affiliation(other, @mock_course)
  end
  def get_affiliation(_any_user, _other_course) do
    {:error, :course_not_exist}
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
