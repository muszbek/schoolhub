defmodule SchoolhubRouter.StripeLib do
  @moduledoc """
  Library module for interacting with Stripe and other payment-related actions.
  """

  alias SchoolhubRouterWeb.Router.Helpers, as: Routes
  alias Stripe.Session
  alias Stripe.BillingPortal.Session, as: BPSession

  def create_session(conn, server_params = %{"owner_email" => email,
					      "price_id" => price_id}) do

    session_data = %{:payment_method_types => ["card"],
		     :mode => "subscription",
		     :success_url => url_prefix() <> Routes.server_path(conn, :success),
		     :cancel_url => url_prefix() <> Routes.server_path(conn, :new),
		     :line_items => [%{:price => price_id,
				       :quantity => 1}],
		     :customer_email => email,
		     :metadata => server_params}
    
    Session.create(session_data)
  end

  def create_billing_portal_session(conn, customer_id) do
    session_data = %{customer: customer_id,
		     return_url: url_prefix() <> Routes.page_path(conn, :index)}
    BPSession.create(session_data)
  end

  def create_billing_portal_session(customer_id) do
    session_data = %{customer: customer_id}
    BPSession.create(session_data)
  end
  
  defp url_prefix() do
    scheme = case Mix.env() do
	       :prod -> "https"
	       _other -> "http"
	     end
    port = ""
    domain = Application.get_env(:schoolhub_router, SchoolhubRouterWeb.Endpoint)[:url]
    |> Keyword.get(:host)
    
    scheme <> "://" <> domain <> port
  end
end
