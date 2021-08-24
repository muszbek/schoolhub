defmodule SchoolhubRouter.Email do
  use Bamboo.Phoenix, view: SchoolhubRouterWeb.EmailView

  alias SchoolhubRouter.Instances
  
  def confirm_reg_email(server) do
    domain = System.get_env("DOMAIN", "localhost")

    %{"name" => name, "address" => address, "owner_email" => email_address} = server
    url = url_prefix() <> domain <> "/" <> address
    
    new_email()
    |> to(email_address)
    |> from("noreply@" <> domain)
    |> subject("Confirm server creation")
    |> assign(:name, name)
    |> assign(:url, url)
    |> render("confirm_reg.text")
  end

  def unsubscribe_email(name, email_address) do
    domain = System.get_env("DOMAIN", "localhost")
    
    token = Instances.create_token(name)
    url = url_prefix() <> domain <> "/router/servers/unsubscribe/" <> token
    
    new_email()
    |> to(email_address)
    |> from("noreply@" <> domain)
    |> subject("Unsubscribe server")
    |> assign(:name, name)
    |> assign(:url_with_token, url)
    |> render("unsubscribe.text")
  end
  

  defp url_prefix() do
    case Mix.env() do
      :prod -> "https://"
      _other -> "http://"
    end
  end
end
