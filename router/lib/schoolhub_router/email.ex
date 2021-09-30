defmodule SchoolhubRouter.Email do
  
  alias SchoolhubRouter.Instances
  require Logger
  
  def confirm_reg_email(server) do
    backend = email_backend()
    [host: domain] = Application.get_env(:schoolhub_router, SchoolhubRouterWeb.Endpoint)[:url]

    %{name: name, address: address, owner_email: email_address} = server
    url = url_prefix() <> domain <> "/" <> address <> "/"

    %{to: email_address,
      from: "noreply@" <> domain,
      subject: "Confirm server creation",
      name_assign: name,
      url_assign: url,
      template: "confirm_reg.text"}
    |> backend.send_email()
  end

  def unsubscribe_email(name, email_address) do
    backend = email_backend()
    [host: domain] = Application.get_env(:schoolhub_router, SchoolhubRouterWeb.Endpoint)[:url]
    
    token = Instances.create_token(name)
    url = url_prefix() <> domain <> "/router/servers/unsubscribe/" <> token
    Logger.info("Email sent out to unsubscribe, referring to url: " <> url)

    %{to: email_address,
      from: "noreply@" <> domain,
      subject: "Unsubscribe server",
      name_assign: name,
      url_assign: url,
      template: "unsubscribe.text"}
    |> backend.send_email()
  end
  

  defp url_prefix() do
    case Mix.env() do
      :prod -> "https://"
      _other -> "http://"
    end
  end

  defp email_backend() do
    case Application.get_env(:schoolhub_router, __MODULE__)[:email_backend] do
      "" -> SchoolhubRouter.Email.Smtp
      _ -> SchoolhubRouter.Email.Http
    end
  end
end
