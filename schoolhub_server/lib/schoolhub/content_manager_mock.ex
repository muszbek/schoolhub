defmodule Schoolhub.ContentManagerMock do
  @moduledoc """
  API for accessing a mock database with mock data.
  Use for testing.
  """

  @mock_teacher 'test_user_teacher'
  @mock_teacher_string "test_user_teacher"
  @mock_student 'test_user_student'
  @mock_student_string "test_user_student"
  @mock_admin 'admin'
  @mock_admin_string "admin"
  @mock_course 'test_course'
  @mock_course_string "test_course"
  @mock_message %{"text" => "test"}

  ### API functions ###
  
  def start_link([]) do
    :ok
  end


  def post_message(@mock_student, @mock_course, @mock_message) do
    {:ok, 1}
  end
  def post_message(@mock_student_string, @mock_course_string, @mock_message) do
    post_message(@mock_student, @mock_course, @mock_message)
  end

  def post_reply(0, _any_student, @mock_course, _any_message) do
    {:error, :origin_not_exist}
  end
  def post_reply(0, any_student, @mock_course_string, any_message) do
    post_reply(0, any_student, @mock_course, any_message)
  end
  def post_reply(id, @mock_student, @mock_course, @mock_message) do
    {:ok, id+1}
  end
  def post_reply(id, @mock_student_string, @mock_course_string, @mock_message) do
    post_reply(id, @mock_student, @mock_course, @mock_message)
  end

  def get_single_message(0, @mock_course) do
    {:error, :message_not_exist}
  end
  def get_single_message(_id, @mock_course) do
    mock_pack_message(@mock_message)
  end
  def get_single_message(id, @mock_course_string) do
    get_single_message(id, @mock_course)
  end

  def delete_single_message(_id, @mock_course) do
    :ok
  end
  def delete_single_message(id, @mock_course_string) do
    delete_single_message(id, @mock_course)
  end

  def modify_single_message(0, @mock_course, _any_user, _any_message) do
    {:error, :message_not_exist}
  end
  def modify_single_message(_id, @mock_course, @mock_teacher, _any_message) do
    :ok
  end
  def modify_single_message(id, @mock_course_string, @mock_teacher_string, any_message) do
    modify_single_message(id, @mock_course, @mock_teacher, any_message)
  end
  def modify_single_message(_id, @mock_course, @mock_admin, _any_message) do
    :ok
  end
  def modify_single_message(id, @mock_course_string, @mock_admin_string, any_message) do
    modify_single_message(id, @mock_course, @mock_admin, any_message)
  end
  def modify_single_message(_id, @mock_course, @mock_student, _any_message) do
    :ok
  end
  def modify_single_message(id, @mock_course_string, @mock_student_string, any_message) do
    modify_single_message(id, @mock_course, @mock_student, any_message)
  end


  defp mock_pack_message(message) do
    %{id: 1,
      course: @mock_course_string,
      author: @mock_student_string,
      ancestor: nil,
      message: message,
      timestamp: "now",
      pinned: false}
  end

end
