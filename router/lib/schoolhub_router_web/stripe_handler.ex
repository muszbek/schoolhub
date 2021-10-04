defmodule SchoolhubRouterWeb.StripeHandler do
  @behaviour Stripe.WebhookHandler

  alias SchoolhubRouter.Instances
  
  @impl true
  def handle_event(%Stripe.Event{type: "checkout.session.completed",
				 data: %{object: event_data_object}}) do

    %{metadata: server_params} = event_data_object
    Instances.commission_server(server_params)
  end

  @impl true
  def handle_event(%Stripe.Event{type: "invoice.paid"} = event) do
    #TODO
    :ok
  end

  @impl true
  def handle_event(%Stripe.Event{type: "invoice.payment_failed"} = event) do
    #TODO
    :ok
  end

  ## Return HTTP 200 for unhandled events
  @impl true
  def handle_event(_event), do: :ok
  
end
