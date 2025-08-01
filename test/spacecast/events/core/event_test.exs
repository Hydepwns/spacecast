defmodule Spacecast.Events.Core.EventTest do
  use Spacecast.DataCase

  alias Spacecast.Events.Core.Event

  describe "create/2" do
    test "creates a valid event" do
      type = "test_event"
      data = %{key: "value"}
      resource_id = "123"
      resource_type = "test_resource"

      assert {:ok, event} =
               Event.create(type, %{
                 resource_id: resource_id,
                 resource_type: resource_type,
                 data: data
               })

      assert event.type == type
      assert event.data == data
      assert event.resource_id == resource_id
      assert event.resource_type == resource_type
      assert is_binary(event.correlation_id)
      assert is_struct(event.timestamp, DateTime)
    end

    test "rejects invalid event type" do
      assert {:error, :invalid_parameters} = Event.create("", %{})
      assert {:error, :invalid_parameters} = Event.create("ab", %{})
      assert {:error, :invalid_parameters} = Event.create(123, %{})
      assert {:error, :invalid_parameters} = Event.create(nil, %{})
    end

    test "rejects invalid attributes" do
      assert {:error, :invalid_parameters} = Event.create("test_event", "not_a_map")
      assert {:error, :invalid_parameters} = Event.create("test_event", nil)
      assert {:error, :invalid_parameters} = Event.create("test_event", 123)
    end

    test "rejects invalid data" do
      changeset = Event.create("test_event", %{data: "not_a_map"})
      assert {:error, changeset} = changeset
      assert "is invalid" in errors_on(changeset).data
    end

    test "rejects invalid metadata" do
      changeset = Event.create("test_event", %{metadata: "not_a_map"})
      assert {:error, changeset} = changeset
      assert "is invalid" in errors_on(changeset).metadata
    end

    test "rejects invalid timestamp" do
      changeset = Event.create("test_event", %{timestamp: "not_a_datetime"})
      assert {:error, changeset} = changeset
      assert "is invalid" in errors_on(changeset).timestamp
    end
  end

  describe "create!/2" do
    test "creates a valid event" do
      type = "test_event"
      data = %{key: "value"}
      resource_id = "123"
      resource_type = "test_resource"

      event =
        Event.create!(type, %{
          resource_id: resource_id,
          resource_type: resource_type,
          data: data
        })

      assert event.type == type
      assert event.data == data
      assert event.resource_id == resource_id
      assert event.resource_type == resource_type
    end

    test "raises on invalid event type" do
      assert_raise ArgumentError, "Invalid event parameters", fn ->
        Event.create!("", %{})
      end

      assert_raise ArgumentError, "Invalid event parameters", fn ->
        Event.create!("ab", %{})
      end

      assert_raise ArgumentError, "Invalid event parameters", fn ->
        Event.create!(123, %{})
      end
    end

    test "raises on invalid attributes" do
      assert_raise ArgumentError, "Invalid event parameters", fn ->
        Event.create!("test_event", "not_a_map")
      end

      assert_raise ArgumentError, "Invalid event parameters", fn ->
        Event.create!("test_event", nil)
      end
    end
  end

  describe "changeset/2" do
    test "creates a valid changeset" do
      attrs = %{
        type: "test_event",
        resource_id: "123",
        resource_type: "test_resource",
        data: %{key: "value"},
        timestamp: DateTime.utc_now()
      }

      changeset = Event.changeset(%Event{}, attrs)
      assert changeset.valid?
    end

    test "rejects invalid attributes" do
      assert {:error, :invalid_attributes} = Event.changeset(%Event{}, "not_a_map")
      assert {:error, :invalid_attributes} = Event.changeset(%Event{}, nil)
      assert {:error, :invalid_attributes} = Event.changeset(%Event{}, 123)
    end

    test "requires type" do
      attrs = %{
        resource_id: "123",
        resource_type: "test_resource",
        data: %{key: "value"}
      }

      changeset = Event.changeset(%Event{}, attrs)
      refute changeset.valid?
      assert "can't be blank" in errors_on(changeset).type
    end

    test "requires resource_id" do
      attrs = %{
        type: "test_event",
        resource_type: "test_resource",
        data: %{key: "value"}
      }

      changeset = Event.changeset(%Event{}, attrs)
      refute changeset.valid?
      assert "can't be blank" in errors_on(changeset).resource_id
    end

    test "requires resource_type" do
      attrs = %{
        type: "test_event",
        resource_id: "123",
        data: %{key: "value"}
      }

      changeset = Event.changeset(%Event{}, attrs)
      refute changeset.valid?
      assert "can't be blank" in errors_on(changeset).resource_type
    end
  end
end
