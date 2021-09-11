defmodule SchoolhubRouter.AdminLib do
  @moduledoc """
  Library module for authorizing admin actions in the router.
  """

  alias Phoenix.Token
  
  def create_token(password) do
    salt = Application.get_env(:schoolhub_router, __MODULE__)[:signing_salt]
    _token = Token.sign(SchoolhubRouterWeb.Endpoint, salt, password)
  end

  def verify_token(token) do
    token
    |> read_token()
    |> verify_password()
  end
  
  defp read_token(token, max_age \\ 60) do
    salt = Application.get_env(:schoolhub_router, __MODULE__)[:signing_salt]
    Token.verify(SchoolhubRouterWeb.Endpoint, salt, token, max_age: max_age)
  end

  defp verify_password({:ok, password}) do
    admin_password = Application.get_env(:schoolhub_router, __MODULE__)[:admin_password]
    case password do
      ^admin_password -> :ok
      _other -> {:error, :wrong_password}
    end
  end
  defp verify_password({:error, error}), do: {:error, error}
  
end
