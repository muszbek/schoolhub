defmodule SchoolhubRouter.Email do
  use Bamboo.Phoenix, view: SchoolhubRouterWeb.EmailView

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

  defp url_prefix() do
    case Mix.env() do
      :prod -> "https://"
      _other -> "http://"
    end
  end
end
