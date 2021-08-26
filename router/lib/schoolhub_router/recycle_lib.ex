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
    ssl_opts = Application.get_env(:schoolhub_router, __MODULE__)[:ssl_opts]
    
    url = "https://" <> address <> "/database/clean"
    options = [ssl: ssl_opts]
    
    case http_impl.get(url, [], options) do
      {:ok, response} -> handle_response(response.status_code)
      {:error, %HTTPoison.Error{}} -> :error
    end
  end

  defp handle_response(200), do: :ok
  defp handle_response(_), do: :error

  defp reset_server(:error), do: :error
  defp reset_server(server) do
    reset_attrs = %{name: nil, owner_email: nil, admin_pw: nil, active: false}
    {:ok, _server} = Instances.update_server(server, reset_attrs)
    :ok
  end
end
