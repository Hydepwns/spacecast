defmodule Spacecast.Resources.OrderResourceTest do
  use Spacecast.DataCase
  alias Spacecast.TestSupport.EventStoreTestHelper
  alias Spacecast.Resources.TestOrderResource

  import Spacecast.Events.ResourceIntegration.EventSourcedResource,
    only: [rebuild_from_events: 3]

  setup do
    EventStoreTestHelper.setup_mock_event_store()
    :ok
  end

  describe "order resource event sourcing" do
    test "creates order and generates created event" do
      order_params = %{
        id: "test-order-1",
        status: "cart",
        items: [],
        customer_id: "customer-1",
        shipping_address: nil,
        billing_address: nil,
        payment_method: nil,
        total_amount: Decimal.new("0.00"),
        tax_amount: Decimal.new("0.00"),
        shipping_amount: Decimal.new("0.00"),
        discount_amount: Decimal.new("0.00"),
        created_at: DateTime.utc_now(),
        updated_at: DateTime.utc_now(),
        fulfilled_at: nil,
        cancelled_at: nil
      }

      {:ok, order} = TestOrderResource.create(order_params)

      assert order.id == "test-order-1"
      assert order.status == "cart"
      assert order.customer_id == "customer-1"
      assert order.items == []
      assert Decimal.eq?(order.total_amount, Decimal.new("0.00"))

      EventStoreTestHelper.assert_event_exists("order.created", TestOrderResource, "test-order-1")
    end

    test "updates order and generates updated event" do
      order_params = %{
        id: "test-order-2",
        status: "cart",
        items: [],
        customer_id: "customer-1",
        shipping_address: nil,
        billing_address: nil,
        payment_method: nil,
        total_amount: Decimal.new("0.00"),
        tax_amount: Decimal.new("0.00"),
        shipping_amount: Decimal.new("0.00"),
        discount_amount: Decimal.new("0.00"),
        created_at: DateTime.utc_now(),
        updated_at: DateTime.utc_now(),
        fulfilled_at: nil,
        cancelled_at: nil
      }

      {:ok, _order} = TestOrderResource.create(order_params)

      update_params = %{
        status: "submitted",
        shipping_address: "123 Main St",
        billing_address: "123 Main St"
      }

      {:ok, updated_order} = TestOrderResource.update("test-order-2", update_params)

      assert updated_order.status == "submitted"
      assert updated_order.shipping_address == "123 Main St"
      assert updated_order.billing_address == "123 Main St"

      EventStoreTestHelper.assert_event_exists("order.updated", TestOrderResource, "test-order-2")
    end

    test "deletes order and generates deleted event" do
      order_params = %{
        id: "test-order-3",
        status: "cart",
        items: [],
        customer_id: "customer-1",
        shipping_address: nil,
        billing_address: nil,
        payment_method: nil,
        total_amount: Decimal.new("0.00"),
        tax_amount: Decimal.new("0.00"),
        shipping_amount: Decimal.new("0.00"),
        discount_amount: Decimal.new("0.00"),
        created_at: DateTime.utc_now(),
        updated_at: DateTime.utc_now(),
        fulfilled_at: nil,
        cancelled_at: nil
      }

      {:ok, _order} = TestOrderResource.create(order_params)
      {:ok, events} = TestOrderResource.delete("test-order-3", %{})

      assert length(events) == 1
      assert hd(events).type == "order.deleted"

      EventStoreTestHelper.assert_event_exists("order.deleted", TestOrderResource, "test-order-3")
    end

    test "rebuilds order state from events" do
      order_params = %{
        id: "test-order-4",
        status: "cart",
        items: [],
        customer_id: "customer-1",
        shipping_address: nil,
        billing_address: nil,
        payment_method: nil,
        total_amount: Decimal.new("0.00"),
        tax_amount: Decimal.new("0.00"),
        shipping_amount: Decimal.new("0.00"),
        discount_amount: Decimal.new("0.00"),
        created_at: DateTime.utc_now(),
        updated_at: DateTime.utc_now(),
        fulfilled_at: nil,
        cancelled_at: nil
      }

      {:ok, _order} = TestOrderResource.create(order_params)
      {:ok, _updated_order} = TestOrderResource.update("test-order-4", %{status: "submitted"})
      {:ok, _events} = TestOrderResource.delete("test-order-4", %{})

      # Get events from the store and rebuild state
      {:ok, events} = TestOrderResource.get_history("test-order-4")

      rebuilt =
        rebuild_from_events(
          events,
          TestOrderResource.initial_state(),
          &TestOrderResource.apply_event/2
        )

      # deleted sets status to cancelled
      assert rebuilt.status == "cancelled"
      assert rebuilt.customer_id == "customer-1"
    end
  end
end
