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
  @mock_admin 'admin'
  @mock_admin_string "admin"

  @scram_prefix "==SCRAM==,"
  @mock_scram "==SCRAM==,jv1SCgihx+Q2yj6PggxUZPbmfp4=,r+T1xjRnDwpUPoC/EwOXA+Jjt2Y=,iCgKQkjMSgfZgjh06UMZzg==,4096"
  ## The password belonging to this mock entry is "test_pw"

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
    get_scram_pw(@mock_user)
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
  def check_user_exist(@mock_admin) do
    true
  end
  def check_user_exist(@mock_admin_string) do
    check_user_exist(@mock_admin)
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
