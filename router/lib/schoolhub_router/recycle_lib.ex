defmodule SchoolhubRouter.RecycleLib do
  @moduledoc """
  Library module for recycling unused server instances.
  """

  alias SchoolhubRouter.Instances
  alias SchoolhubRouter.Instances.Server

  def recycle_server(server) do
    server
    |> clean_database()
    |> reset_server()
  end
  
  defp clean_database(server = %Server{}) do
    case clean_database(server.address) do
      :ok -> server
      :error -> :error
    end
  end
  
  defp clean_database(address) when is_binary(address) do
    http_impl = Application.get_env(:schoolhub_router, __MODULE__)[:http_impl]
    phx_port = Application.get_env(:schoolhub_router, __MODULE__)[:phx_port] |> to_string()
    ssl_opts = Application.get_env(:schoolhub_router, __MODULE__)[:ssl_opts]
    
    url = "https://" <> address <> ":" <> phx_port <> "/database/clean"
    options = [ssl: ssl_opts]
    
    case http_impl.get(url, [], options) do
      {:ok, response} -> handle_response(response.status_code)
      {:error, %HTTPoison.Error{}} -> :error
    end
  end

  defp handle_response(200), do: :ok
  defp handle_response(302), do: :ok
  defp handle_response(_code), do: :error

  defp reset_server(:error), do: :error
  defp reset_server(server) do
    reset_attrs = %{active: false, customer_id: nil, last_paid: nil, last_unpaid: nil}
    {:ok, _server} = Instances.update_server(server, reset_attrs)
    :ok
  end

  
  def insert_admin(server = %Server{}) do
    http_impl = Application.get_env(:schoolhub_router, __MODULE__)[:http_impl]
    phx_port = Application.get_env(:schoolhub_router, __MODULE__)[:phx_port] |> to_string()
    ssl_opts = Application.get_env(:schoolhub_router, __MODULE__)[:ssl_opts]
    
    url = "https://" <> server.address <> ":" <> phx_port <> "/database/insert_admin"
    options = [ssl: ssl_opts]
    
    http_impl.get(url, [], options)
  end
end
