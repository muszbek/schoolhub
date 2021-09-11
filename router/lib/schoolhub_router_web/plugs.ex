defmodule SchoolhubRouterWeb.Plugs do
  use SchoolhubRouterWeb, :controller

  alias SchoolhubRouter.AdminLib

  def authorize_admin(conn, _) do
    case verify_cookie(conn) do
      :ok -> conn
      {:error, _error} ->
	conn
	|> Phoenix.Controller.put_flash(:error, "Admin authorization failed")
	|> Phoenix.Controller.redirect(to: Routes.page_path(conn, :index))
	|> halt()
    end
  end

  defp verify_cookie(conn) do
    case token_from_cookie(conn) do
      nil -> {:error, :no_token}
      "" -> {:error, :no_token}
      token -> AdminLib.verify_token(token)
    end
  end

  defp token_from_cookie(conn) do
    conn
    |> fetch_cookies()
    |> Map.from_struct()
    |> get_in([:cookies, "admin_token"])
  end
end
