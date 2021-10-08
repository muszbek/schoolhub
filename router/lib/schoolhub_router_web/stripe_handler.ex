defmodule SchoolhubRouterWeb.StripeHandler do
  @behaviour Stripe.WebhookHandler

  alias SchoolhubRouter.Instances
  alias SchoolhubRouter.RecycleLib
  
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
  def handle_event(%Stripe.Event{type: "invoice.paid",
				 data: %{object: event_data_object}}) do

    payment_data = %{last_paid: DateTime.utc_now, last_unpaid: nil}
    %{customer: customer_id} = event_data_object

    case Instances.get_server_by_customer(customer_id) do
      nil -> {:ok, :server_not_yet_created}
      server -> Instances.update_server(server, payment_data)
    end
  end

  @impl true
  def handle_event(%Stripe.Event{type: "invoice.payment_failed",
				 data: %{object: event_data_object}}) do
    
    payment_data = %{last_unpaid: DateTime.utc_now}
    %{customer: customer_id} = event_data_object

    customer_id
    |> Instances.get_server_by_customer()
    |> Instances.update_server(payment_data)
  end

  @impl true
  def handle_event(%Stripe.Event{type: "customer.subscription.deleted",
				 data: %{object: event_data_object}}) do
    
    %{customer: customer_id} = event_data_object
    
    case Instances.get_server_by_customer(customer_id) do
      nil -> {:ok, :server_already_recycled}
      server -> RecycleLib.recycle_server(server)
    end
  end

  ## Return HTTP 200 for unhandled events
  @impl true
  def handle_event(_event), do: :ok
  
end
