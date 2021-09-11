defmodule SchoolhubRouterWeb.Plugs do
  use SchoolhubRouterWeb, :controller

  alias SchoolhubRouter.AdminLib

  def authorize_admin(conn, _) do
    case verify_header(conn) do
      :ok -> conn
      {:error, _error} ->
	conn
	|> Phoenix.Controller.put_flash(:error, "Admin authorization failed")
	|> Phoenix.Controller.redirect(to: Routes.page_path(conn, :index))
	|> halt()
    end
  end

  defp verify_header(conn) do
    case get_req_header(conn, "admin_auth") do
      [] -> {:error, :no_token}
      [token] -> AdminLib.verify_token(token)
    end
  end
end
