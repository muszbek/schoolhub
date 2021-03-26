defmodule Schoolhub.Email do
  use Bamboo.Phoenix, view: SchoolhubWeb.EmailView

  def forgot_pw_email(address) do
    domain = System.get_env("DOMAIN", "localhost")
    
    new_email(
      to: address,
      from: "support@" <> domain,
      subject: "Forgot password",
      html_body: "That's too bad :(",
      text_body: "That's too bad :("
    )
  end
end
