defmodule Schoolhub.DataManagerMock do
  @moduledoc """
  API for accessing a mock database with mock data.
  Use for testing.
  """

  @mock_user 'test_user'
  @mock_scram "==SCRAM==,jv1SCgihx+Q2yj6PggxUZPbmfp4=,r+T1xjRnDwpUPoC/EwOXA+Jjt2Y=,iCgKQkjMSgfZgjh06UMZzg==,4096"
  ## The password belonging to this mock entry is "test_pw"

  ### API functions ###
  
  def start_link([]) do
    :ok
  end

  
  def get_scram_pw(@mock_user) do
    @mock_scram
  end

  def get_scram_pw(_other_user) do
    :nil
  end
  
end
