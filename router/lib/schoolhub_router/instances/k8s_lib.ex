defmodule SchoolhubRouter.Instances.K8sLib do
  @moduledoc """
  Library module for interacting with the Kubernetes API.
  """

  # require Logger
  
  def connect() do
    {:ok, _conn} = K8s.Conn.from_service_account()
  end

  def get_scale(conn) do
    operation = K8s.Client.get("apps/v1", "statefulsets/scale",
      [name: "schoolhub-instance", namespace: "default"])
    {:ok, _scale} = K8s.Client.run(conn, operation)
  end

  def scale(conn, amount) do
    # {:ok, scale} = get_scale(conn)
    # new_scale = %{scale | "spec" => %{"replicas" => amount}}
    new_scale = %{"spec" => %{"replicas" => amount}}
    operation = K8s.Client.patch("apps/v1", "statefulsets/scale",
      [name: "schoolhub-instance", namespace: "default"], new_scale)
    {:ok, _scale} = K8s.Client.run(conn, operation)
  end

end
