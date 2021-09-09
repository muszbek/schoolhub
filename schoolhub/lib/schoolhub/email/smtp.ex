defmodule Schoolhub.Email.Smtp do
  use Bamboo.Phoenix, view: SchoolhubWeb.EmailView

  alias Schoolhub.Mailer

  def send_email(struct) do
    struct
    |> create_email()
    |> Mailer.deliver_now!()
  end
  
  defp create_email(%{to: to,
		      from: from,
		      subject: subject,
		      username_assign: username_assign,
		      url_assign: url_assign,
		      template: template}) do
    
    new_email()
    |> to(to)
    |> from(from)
    |> subject(subject)
    |> assign(:username, username_assign)
    |> assign(:url_with_token, url_assign)
    |> render(template)
  end
end
