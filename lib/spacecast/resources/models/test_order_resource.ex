defmodule Spacecast.Resources.TestOrderResource do
  @moduledoc """
  Test-only event-sourced resource for testing the OrderResource behavior.

  This module is used exclusively for testing and should not be used in production code.
  """

  use Spacecast.Events.ResourceIntegration.EventSourcedResource

  def initial_state do
    %{
      id: nil,
      status: "cart",
      items: [],
      customer_id: nil,
      shipping_address: nil,
      billing_address: nil,
      payment_method: nil,
      total_amount: Decimal.new("0.00"),
      tax_amount: Decimal.new("0.00"),
      shipping_amount: Decimal.new("0.00"),
      discount_amount: Decimal.new("0.00"),
      created_at: nil,
      updated_at: nil,
      fulfilled_at: nil,
      cancelled_at: nil
    }
  end

  # Required by the macro - note: create_events/2 (id, params)
  def create_events(id, params) do
    resource_id = id || params[:id] || "test-order-#{System.unique_integer()}"

    {:ok,
     [
       %{
         type: "order.created",
         data: params,
         resource_id: resource_id,
         resource_type: resource_type()
       }
     ]}
  end

  def create_update_events(resource, params) do
    # The resource parameter is the event data, extract the ID from it
    resource_id = resource[:id] || resource[:resource_id] || "unknown"

    [
      %{
        type: "order.updated",
        data: params,
        resource_id: resource_id,
        resource_type: resource_type()
      }
    ]
  end

  def create_delete_events(id, _params) do
    {:ok,
     [
       %{type: "order.deleted", data: %{}, resource_id: id, resource_type: resource_type()}
     ]}
  end

  def apply_event(%{type: "order.created", data: data}, state), do: Map.merge(state, data)
  def apply_event(%{type: "order.updated", data: data}, state), do: Map.merge(state, data)
  def apply_event(%{type: "order.deleted"}, state), do: Map.put(state, :status, "cancelled")
  def apply_event(_event, state), do: state
end
