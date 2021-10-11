defmodule SchoolhubRouterWeb.StripeHandlerTest do
  use SchoolhubRouterWeb.ConnCase

  alias SchoolhubRouterWeb.StripeHandler
  alias SchoolhubRouter.Instances

  @customer "some_customer_id"
  @server_params %{name: "some_name", admin_pw: "some_pw", owner_email: "some_email"}
  @invalid_server_params %{name: nil, admin_pw: nil, owner_email: nil}

  def fixture(:server) do
    event = %Stripe.Event{type: "checkout.session.completed",
			    data: %{object: %{customer: @customer,
					      metadata: @server_params}}}
    StripeHandler.handle_event(event)
  end

  
  describe "handle_event checkout.session.completed" do
    test "returns ok with correct data" do
      event = %Stripe.Event{type: "checkout.session.completed",
			    data: %{object: %{customer: @customer,
					      metadata: @server_params}}}
      
      assert :ok = StripeHandler.handle_event(event)
      server = Instances.get_server_by_customer(@customer)
      assert server.customer_id == @customer
      assert server.last_paid != nil
      assert server.last_unpaid == nil
    end

    test "returns error changeset with incorrect data" do
      event = %Stripe.Event{type: "checkout.session.completed",
			    data: %{object: %{customer: @customer,
					      metadata: @invalid_server_params}}}
      
      assert {:error, %Ecto.Changeset{}} = StripeHandler.handle_event(event)
    end
  end

  describe "handle_event invoice.paid" do
    test "returns ok when server is present" do
      create_server(:something)

      event = %Stripe.Event{type: "invoice.paid",
			    data: %{object: %{customer: @customer}}}

      assert {:ok, %{}} = StripeHandler.handle_event(event)
      server = Instances.get_server_by_customer(@customer)
      assert server.last_paid != nil
      assert server.last_unpaid == nil
    end

    test "returns ok when server is not yet present" do
      ## This typically occurs upon subscription, initial payment
      event = %Stripe.Event{type: "invoice.paid",
			    data: %{object: %{customer: @customer}}}

      assert {:ok, :server_not_yet_created} = StripeHandler.handle_event(event)
    end
  end

  describe "handle_event invoice.payment_failed" do
    setup [:create_server]

    test "returns ok when server is present" do
      event = %Stripe.Event{type: "invoice.payment_failed",
			    data: %{object: %{customer: @customer}}}

      assert {:ok, %{}} = StripeHandler.handle_event(event)
      server = Instances.get_server_by_customer(@customer)
      assert server.last_unpaid != nil
    end
  end

  describe "handle_event customer.subscription.deleted" do

    test "returns ok when server is present" do
      create_server(:something)
      
      event = %Stripe.Event{type: "customer.subscription.deleted",
			    data: %{object: %{customer: @customer}}}

      server_before_recycle = Instances.get_server_by_customer(@customer)
      assert :ok = StripeHandler.handle_event(event)
      server = Instances.get_server!(server_before_recycle.id)
      assert server.customer_id == nil
      assert server.last_paid == nil
      assert server.last_unpaid == nil
    end

    test "returns ok when server is not present" do
      event = %Stripe.Event{type: "customer.subscription.deleted",
			    data: %{object: %{customer: @customer}}}

      assert {:ok, :server_already_recycled} = StripeHandler.handle_event(event)
    end
  end


  defp create_server(_) do
    server = fixture(:server)
    %{server: server}
  end
end
