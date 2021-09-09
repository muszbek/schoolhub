defmodule SchoolhubRouter.Email do
  
  alias SchoolhubRouter.Instances
  alias SchoolhubRouter.Email.Smtp
  require Logger
  
  def confirm_reg_email(server) do
    domain = System.get_env("DOMAIN", "localhost")

    %{name: name, address: address, owner_email: email_address} = server
    url = url_prefix() <> domain <> "/" <> address <> "/"

    %{to: email_address,
      from: "noreply@" <> domain,
      subject: "Confirm server creation",
      name_assign: name,
      url_assign: url,
      template: "confirm_reg.text"}
    |> Smtp.send_email()
  end

  def unsubscribe_email(name, email_address) do
    domain = System.get_env("DOMAIN", "localhost")
    
    token = Instances.create_token(name)
    url = url_prefix() <> domain <> "/router/servers/unsubscribe/" <> token
    Logger.info("Email sent out to unsubscribe, referring to url: " <> url)

    %{to: email_address,
      from: "noreply@" <> domain,
      subject: "Unsubscribe server",
      name_assign: name,
      url_assign: url,
      template: "unsubscribe.text"}
    |> Smtp.send_email()
  end
  

  defp url_prefix() do
    case Mix.env() do
      :prod -> "https://"
      _other -> "http://"
    end
  end
end
