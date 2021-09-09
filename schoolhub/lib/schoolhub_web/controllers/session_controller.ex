defmodule SchoolhubWeb.SessionController do
  use SchoolhubWeb, :controller
  
  alias Schoolhub.Accounts
  alias Schoolhub.AuthServer
  alias Schoolhub.Email
  
  @http_response_timeout 1_000
  

  def new(conn, _) do
    render(conn, "new.html")
  end

  def create(conn, form_data = %{"username" => username, "result" => result}) do    
    case Accounts.authenticate(result, username) do
      {:ok, user} ->
        conn
        |> put_flash(:info, "Welcome back!")
	|> enter_session(user)
	|> add_access_token(form_data)
	|> add_refresh_token(form_data)
        |> redirect(to: Routing.route(:page_path, conn, [:index]))
      {:error, :unauthorized} ->
        conn
        |> put_flash(:error, "Bad username/password combination")
        |> redirect(to: Routing.route(:session_path, conn, [:new]))
    end
  end

  def delete(conn, _) do
    conn
    |> put_flash(:info, "Logged out")
    |> configure_session(drop: true)
    |> redirect(to: Routing.route(:session_path, conn, [:new]))
  end

  def forgot_pw(conn, _) do
    render(conn, "forgot_pw.html")
  end

  def send_email(conn, _form_data = %{"email" => address}) do
    case Accounts.get_user_by_email(address) do
      nil ->
	conn
	|> put_flash(:error, "Email address not found in database, please register")
	|> redirect(to: Routing.route(:session_path, conn, [:new]))

      user ->
	conn
	|> Email.forgot_pw_email(user)
    
	conn
	|> put_flash(:info, "Email sent")
	|> redirect(to: Routing.route(:session_path, conn, [:new]))
    end
  end

  
  def authenticate(conn, %{"data" => auth_data}) do
    AuthServer.authenticate(auth_data)
    conn
    |> put_resp_content_type("application/json")
    |> http_respond()
  end


  def renew_token(conn, data) do
    conn
    |> add_refresh_token(data)
    |> send_resp(200, "new_token_stored")
  end


  def enter_session(conn, user) do
    conn
    |> put_session(:user_id, user.id)
    |> configure_session(renew: true)
  end
  

  defp add_access_token(conn, form_data) do
    %{"access_token" => access_token} = form_data
    put_session(conn, :access_token, access_token)
  end

  defp add_refresh_token(conn, form_data) do
    %{"refresh_token" => refresh_token} = form_data
    put_session(conn, :refresh_token, refresh_token)
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
