defmodule SchoolhubRouter.Email.Smtp do
  use Bamboo.Phoenix, view: SchoolhubRouterWeb.EmailView

  alias SchoolhubRouter.Mailer

  def send_email(struct) do
    struct
    |> create_email()
    |> Mailer.deliver_now!()
  end
  
  defp create_email(%{to: to,
		      from: from,
		      subject: subject,
		      name_assign: name_assign,
		      url_assign: url_assign,
		      template: template}) do
    
    new_email()
    |> to(to)
    |> from(from)
    |> subject(subject)
    |> assign(:name, name_assign)
    |> assign(:url, url_assign)
    |> render(template)
  end
end
