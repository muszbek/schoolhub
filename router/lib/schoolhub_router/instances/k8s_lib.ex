defmodule SchoolhubRouter.Instances.K8sLib do
  @moduledoc """
  Library module for interacting with the Kubernetes API.
  """

  # require Logger
  
  def connect() do
    K8s.Conn.from_service_account()
  end

  def get_scale(conn) do
    operation = K8s.Client.get("apps/v1", "statefulsets/scale",
      [name: "schoolhub-instance", namespace: "default"])
    K8s.Client.run(conn, operation)
  end

  def scale({:ok, conn}, amount) do
    new_scale = %{"spec" => %{"replicas" => amount}}
    operation = K8s.Client.patch("apps/v1", "statefulsets/scale",
      [name: "schoolhub-instance", namespace: "default"], new_scale)
    K8s.Client.run(conn, operation)
  end
  def scale({:error, error}, _amount), do: {:error, error}

end
