defmodule SchoolhubRouter.StripeLib do
  @moduledoc """
  Library module for interacting with Stripe and other payment-related actions.
  """

  alias SchoolhubRouterWeb.Router.Helpers, as: Routes
  alias Stripe.Session

  def create_session(conn, _server_params = %{"owner_email" => email,
					      "priceId" => price_id}) do
    
    Session.create(%{:payment_method_types => ["card"],
		     :mode => "subscription",
		     :success_url => url_prefix() <> Routes.page_path(conn, :index),
		     :cancel_url => url_prefix() <> Routes.server_path(conn, :new),
		     :line_items => [%{:price => price_id,
				       :quantity => 1}],
		     :customer_email => email})
  end
  
  defp url_prefix() do
    scheme = case Mix.env() do
	       :prod -> "https://"
	       _other -> "http://"
	     end
    port = ""
    [host: domain] = Application.get_env(:schoolhub_router, SchoolhubRouterWeb.Endpoint)[:url]
    scheme <> "://" <> domain <> port <> "/"
  end
end
