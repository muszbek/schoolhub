defmodule Schoolhub.Email do
  use Bamboo.Phoenix, view: SchoolhubWeb.EmailView

  alias Schoolhub.Accounts
  require Logger
  def forgot_pw_email(user) do
    domain = System.get_env("DOMAIN", "localhost")
    
    username = user.credential.username
    address = user.email
    token = Accounts.create_token(username)
    url = url_prefix() <> domain <> "/users/change_pw/" <> token
    
    new_email()
    |> to(address)
    |> from("noreply@" <> domain)
    |> subject("Forgot password")
    |> assign(:username, username)
    |> assign(:url_with_token, url)
    |> render("forgot_pw.text")
  end
  
  def confirm_reg_email(attrs) do
    domain = System.get_env("DOMAIN", "localhost")

    %{"email" => address, "credential" => %{"username" => username}} = attrs
    token = Accounts.create_token(attrs)
    url = url_prefix() <> domain <> "/users/register/" <> token
    
    new_email()
    |> to(address)
    |> from("noreply@" <> domain)
    |> subject("Confirm registration")
    |> assign(:username, username)
    |> assign(:url_with_token, url)
    |> render("confirm_reg.text")
  end

  defp url_prefix() do
    case Mix.env() do
      :prod -> "https://"
      _other -> "http://"
    end
  end
end
