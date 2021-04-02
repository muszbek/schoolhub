defmodule Schoolhub.Email do
  use Bamboo.Phoenix, view: SchoolhubWeb.EmailView

  alias Schoolhub.Accounts
  
  def forgot_pw_email(user) do
    domain = System.get_env("DOMAIN", "localhost")
    
    username = user.credential.username
    address = user.email
    token = Accounts.create_token(username)
    url = domain <> "/users/change_pw/" <> token
    
    new_email()
    |> to(address)
    |> from("support@" <> domain)
    |> subject("Forgot password")
    |> assign(:username, username)
    |> assign(:url_with_token, url)
    |> render("forgot_pw.text")
  end
end
