defmodule SchoolhubRouter.Instances.K8sMock do
  @moduledoc """
  Test helper module for mocking the calls on K8sLib,
  replacing calls to the Kubernetes API.
  """

  def connect() do
    {:ok, :conn_mock}
  end

  def get_scale({:ok, _conn}) do
    {:ok, :scale_mock}
  end

  def scale({:ok, _conn}, _amount) do
    {:ok, %{"kind" => "Scale"}}
  end
  def scale({:error, error}, _amount), do: {:error, error}
end
