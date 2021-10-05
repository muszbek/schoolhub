defmodule SchoolhubRouterWeb.StripeHandler do
  @behaviour Stripe.WebhookHandler

  alias SchoolhubRouter.Instances
  
  @impl true
  def handle_event(%Stripe.Event{type: "checkout.session.completed",
				 data: %{object: event_data_object}}) do

    %{customer: customer_id, metadata: server_params} = event_data_object
    
    server_params
    |> Map.put("customer_id", customer_id)
    |> Map.put("last_paid", DateTime.utc_now)
    |> Instances.commission_server()
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
