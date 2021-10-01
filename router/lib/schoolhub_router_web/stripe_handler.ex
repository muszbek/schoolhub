defmodule SchoolhubRouterWeb.StripeHandler do
  @behaviour Stripe.WebhookHandler

  @impl true
  def handle_event(%Stripe.Event{type: "checkout.session.completed"} = event) do
    #TODO
    :ok
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
