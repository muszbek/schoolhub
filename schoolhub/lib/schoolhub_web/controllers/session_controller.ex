defmodule SchoolhubWeb.SessionController do
  use SchoolhubWeb, :controller
  
  alias Schoolhub.Accounts
  alias Schoolhub.AuthServer
  
  @http_response_timeout 1_000
  

  def new(conn, _) do
    render(conn, "new.html")
  end

  def create(conn, %{"username" => username, "result" => result}) do    
    case Accounts.authenticate(result, username) do
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
    |> put_flash(:info, "Logged out")
    |> configure_session(drop: true)
    |> redirect(to: Routes.session_path(conn, :new))
  end

  
  def authenticate(conn, %{"data" => auth_data}) do
    AuthServer.authenticate(auth_data)
    conn
    |> put_resp_content_type("application/json")
    |> http_respond()
  end
  
  
  defp http_respond(conn) do
    receive do
      {:reply, response} ->
	{:ok, response_body} = Jason.encode(%{data: to_string(response)})
	send_resp(conn, 200, response_body)
    after
      @http_response_timeout ->
	raise "http_timeout"
    end	
  end
  
end
