defmodule SchoolhubRouter.Email.Http do

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
		     name_assign: name_assign,
		     url_assign: url_assign,
		     template: template}) do

    text_module = SchoolhubRouterWeb.EmailView
    text = View.render_to_string(text_module, template,
      name: name_assign, url: url_assign)
      
    %{"Messages" => [
      %{"From" => %{"Email" => from},
	"To" => [%{"Email" => to}],
	"Subject" => subject,
	"TextPart" => text}
    ]}
    |> Jason.encode!()
  end

  defp deliver_now!(body) do
    url = "https://api.mailjet.com/v3.1/send"
    creds = Application.get_env(:schoolhub_router, __MODULE__)[:mailjet_creds]
    |> Base.encode64
    headers = %{"Content-Type" => "application/json",
		"Authorization" => "Basic " <> creds}
    response = HTTPoison.post!(url, body, headers)
    
    Logger.info("Mailjet response to email API request: " <> inspect(response))
  end
end
