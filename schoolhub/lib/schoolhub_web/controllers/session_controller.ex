defmodule SchoolhubWeb.SessionController do
  use SchoolhubWeb, :controller

  alias Schoolhub.Accounts
  alias Schoolhub.AuthServer
  
  @http_response_timeout 1_000
  

  def new(conn, _) do
    render(conn, "new.html")
  end

  def create(conn, %{"credential" => %{"username" => username, "password" => password}}) do
    case Accounts.authenticate(username, password) do
      {:ok, user} ->
        conn
        |> put_flash(:info, "Welcome back!")
        |> put_session(:user_id, user.id)
        |> configure_session(renew: true)
        |> redirect(to: "/")
      {:error, :unauthorized} ->
        conn
        |> put_flash(:error, "Bad username/password combination")
        |> redirect(to: Routes.session_path(conn, :new))
    end
  end

  def delete(conn, _) do
    conn
    |> configure_session(drop: true)
    |> redirect(to: "/")
  end

  
  def authenticate(conn, %{"data" => auth_data}) do
    AuthServer.authenticate(auth_data)
    http_respond(conn)
  end
  
  
  defp http_respond(conn) do
    receive do
      {:reply, response_body} ->
	send_resp(conn, 200, response_body)
    after
      @http_response_timeout ->
	raise "http_timeout"
    end	
  end
  
end
