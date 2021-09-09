defmodule Schoolhub.Email.Http do

  alias Phoenix.View
  require Logger
  
  def send_email(struct) do
    struct
    |> create_body()
    |> deliver_now!()
  end
  
  defp create_body(%{to: to,
		     from: from,
		     subject: subject,
		     username_assign: username_assign,
		     url_assign: url_assign,
		     template: template}) do

    text_module = SchoolhubWeb.EmailView
    text = View.render_to_string(text_module, template,
      username: username_assign, url_with_token: url_assign)
      
    %{"Messages" => [
      %{"From" => %{"From" => from},
	"To" => [%{"Email" => to}],
	"Subject" => subject,
	"TextPart" => text}
    ]}
    |> Jason.encode!()
  end

  defp deliver_now!(body) do
    url = "https://api.mailjet.com/v3.1/send"
    creds = Application.get_env(:schoolhub, __MODULE__)[:mailjet_creds]
    |> Base.encode64
    headers = %{"Content-Type" => "application/json",
		"Authorization" => "Basic " <> creds}
    response = HTTPoison.post!(url, body, headers)
    
    Logger.info("Mailjet response to email API request: " <> inspect(response))
  end
end
