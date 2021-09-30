defmodule Schoolhub.Email do

  alias Schoolhub.Accounts
  alias SchoolhubWeb.Routing
  
  def forgot_pw_email(conn, user) do
    backend = email_backend()
    [host: domain] = Application.get_env(:schoolhub, SchoolhubWeb.Endpoint)[:url]
    internal_host = Routing.internal_host(conn)
    
    username = user.credential.username
    address = user.email
    token = Accounts.create_token(username)
    url = url_prefix() <> domain <> internal_host <> "/users/change_pw/" <> token

    %{to: address,
      from: "noreply@" <> domain,
      subject: "Forgot password",
      username_assign: username,
      url_assign: url,
      template: "forgot_pw.text"}
    |> backend.send_email()
  end
  
  def confirm_reg_email(conn, attrs) do
    backend = email_backend()
    [host: domain] = Application.get_env(:schoolhub, SchoolhubWeb.Endpoint)[:url]
    internal_host = Routing.internal_host(conn)

    %{"email" => address, "credential" => %{"username" => username}} = attrs
    token = Accounts.create_token(attrs)
    url = url_prefix() <> domain <> internal_host <> "/users/register/" <> token

    %{to: address,
      from: "noreply@" <> domain,
      subject: "Confirm registration",
      username_assign: username,
      url_assign: url,
      template: "confirm_reg.text"}
    |> backend.send_email()
  end


  defp url_prefix() do
    case Mix.env() do
      :prod -> "https://"
      _other -> "http://"
    end
  end

  defp email_backend() do
    case Application.get_env(:schoolhub, __MODULE__)[:email_backend] do
      "" -> Schoolhub.Email.Smtp
      _ -> Schoolhub.Email.Http
    end
  end
end
