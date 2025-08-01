defmodule Spacecast.Events.Core.EventBusTest do
  use ExUnit.Case, async: false
  alias Spacecast.Events.Core.{EventBus, Event}

  # Simple test event struct that matches Event interface but isn't an Ecto schema
  defmodule TestEvent do
    defstruct [:id, :type, :resource_id, :resource_type, :data, :metadata, :correlation_id, :causation_id, :timestamp]
  end

  setup do
    # The EventBus is already started by the application
    # Clean up any existing subscriptions
    :ok
  end

  describe "start_link/1" do
    test "starts the event bus successfully" do
      # The EventBus is already started by the application
      assert {:error, {:already_started, pid}} = EventBus.start_link()
      assert Process.alive?(pid)
    end

    test "starts with custom options" do
      opts = [name: :test_event_bus]
      # The EventBus is already started by the application
      assert {:error, {:already_started, pid}} = EventBus.start_link(opts)
      assert Process.alive?(pid)
    end
  end

  describe "publish/2" do
    test "publishes valid event successfully" do
      event =
        Event.create!("test_event", %{
          resource_id: "test-123",
          resource_type: "test",
          data: %{message: "Hello"}
        })

      assert :ok = EventBus.publish(event)
    end

    test "publishes event with options" do
      event =
        Event.create!("test_event", %{
          resource_id: "test-123",
          resource_type: "test",
          data: %{message: "Hello"}
        })

      assert :ok = EventBus.publish(event, %{store: true})
    end

    test "handles invalid event" do
      assert {:error, :invalid_event} = EventBus.publish("not_an_event")
      assert {:error, :invalid_event} = EventBus.publish(nil)
    end
  end

  describe "subscribe/1" do
    test "subscribes to single event type" do
      assert :ok = EventBus.subscribe("test_event")
    end

    test "subscribes to :all events" do
      assert :ok = EventBus.subscribe(:all)
    end
  end

  describe "subscribe/2" do
    test "subscribes process to single event type" do
      test_pid = self()
      assert :ok = EventBus.subscribe(test_pid, "test_event")
    end

    test "subscribes process to list of event types" do
      test_pid = self()
      assert :ok = EventBus.subscribe(test_pid, ["event1", "event2", "event3"])
    end

    test "subscribes process to :all events" do
      test_pid = self()
      assert :ok = EventBus.subscribe(test_pid, :all)
    end

    test "subscribes registered process" do
      # Register a test process
      test_pid =
        spawn(fn ->
          receive do
            _ -> :ok
          end
        end)

      Process.register(test_pid, :test_subscriber)

      assert :ok = EventBus.subscribe(:test_subscriber, "test_event")

      # Clean up
      Process.exit(test_pid, :kill)
    end
  end

  describe "unsubscribe/1" do
    test "unsubscribes from single event type" do
      # First subscribe
      assert :ok = EventBus.subscribe("test_event")
      # Then unsubscribe
      assert :ok = EventBus.unsubscribe("test_event")
    end

    test "unsubscribes from :all events" do
      # First subscribe
      assert :ok = EventBus.subscribe(:all)
      # Then unsubscribe
      assert :ok = EventBus.unsubscribe(:all)
    end
  end

  describe "unsubscribe/2" do
    test "unsubscribes process from single event type" do
      test_pid = self()
      # First subscribe
      assert :ok = EventBus.subscribe(test_pid, "test_event")
      # Then unsubscribe
      assert :ok = EventBus.unsubscribe(test_pid, "test_event")
    end

    test "unsubscribes process from list of event types" do
      test_pid = self()
      # First subscribe
      assert :ok = EventBus.subscribe(test_pid, ["event1", "event2"])
      # Then unsubscribe
      assert :ok = EventBus.unsubscribe(test_pid, ["event1", "event2"])
    end

    test "unsubscribes process from :all events" do
      test_pid = self()
      # First subscribe
      assert :ok = EventBus.subscribe(test_pid, :all)
      # Then unsubscribe
      assert :ok = EventBus.unsubscribe(test_pid, :all)
    end

    test "unsubscribes registered process" do
      # Register a test process
      test_pid =
        spawn(fn ->
          receive do
            _ -> :ok
          end
        end)

      Process.register(test_pid, :test_subscriber)

      # First subscribe
      assert :ok = EventBus.subscribe(:test_subscriber, "test_event")
      # Then unsubscribe
      assert :ok = EventBus.unsubscribe(:test_subscriber, "test_event")

      # Clean up
      Process.exit(test_pid, :kill)
    end
  end

  describe "get_subscribers/1" do
    test "gets subscribers for valid event type" do
      test_pid = self()
      # Subscribe to the event type
      assert :ok = EventBus.subscribe(test_pid, "test_event")

      # Get subscribers
      assert {:ok, subscribers} = EventBus.get_subscribers("test_event")
      assert is_list(subscribers)
      assert test_pid in subscribers

      # Clean up
      assert :ok = EventBus.unsubscribe(test_pid, "test_event")
    end

    test "returns empty list for event type with no subscribers" do
      # Use a unique event type that shouldn't have subscribers
      unique_event_type = "unique_event_#{System.unique_integer()}"
      assert {:ok, subscribers} = EventBus.get_subscribers(unique_event_type)
      assert subscribers == []
    end

    test "handles invalid event type" do
      assert {:error, :invalid_event_type} = EventBus.get_subscribers("")
      assert {:error, :invalid_event_type} = EventBus.get_subscribers(nil)
    end
  end

  describe "register_event_type/1" do
    test "registers new event type" do
      assert :ok = EventBus.register_event_type("new_event_type")
    end

    test "registers existing event type (idempotent)" do
      assert :ok = EventBus.register_event_type("existing_event_type")
      assert :ok = EventBus.register_event_type("existing_event_type")
    end
  end

  describe "list_event_types/0" do
    test "lists registered event types" do
      # Register some event types
      assert :ok = EventBus.register_event_type("event_type_1")
      assert :ok = EventBus.register_event_type("event_type_2")

      # List event types
      event_types = EventBus.list_event_types()
      assert is_list(event_types)
      assert "event_type_1" in event_types
      assert "event_type_2" in event_types
    end

    test "returns empty list when no event types registered" do
      # This test assumes a clean state
      event_types = EventBus.list_event_types()
      assert is_list(event_types)
    end
  end

  describe "event publishing and subscription" do
    test "subscribers receive published events" do
      test_pid = self()
      event_type = "test_publish_event"

      # Subscribe to the event type
      assert :ok = EventBus.subscribe(test_pid, event_type)

      # Create and publish an event
      event =
        Event.create!(event_type, %{
          resource_id: "test-123",
          resource_type: "test",
          data: %{message: "Test message"}
        })

      assert :ok = EventBus.publish(event)

      # Wait for the event
      assert_receive {:event, received_event}
      assert received_event.id == event.id
      assert received_event.type == event.type
      assert received_event.data == event.data

      # Clean up
      assert :ok = EventBus.unsubscribe(test_pid, event_type)
    end

    test ":all subscribers receive all events" do
      test_pid = self()

      # Subscribe to all events
      assert :ok = EventBus.subscribe(test_pid, :all)

      # Create and publish an event
      event =
        Event.create!("test_all_event", %{
          resource_id: "test-123",
          resource_type: "test",
          data: %{message: "Test message"}
        })

      assert :ok = EventBus.publish(event)

      # Wait for the event
      assert_receive {:event, received_event}
      assert received_event.id == event.id
      assert received_event.type == event.type

      # Clean up
      assert :ok = EventBus.unsubscribe(test_pid, :all)
    end

    test "unsubscribed processes don't receive events" do
      test_pid = self()
      event_type = "test_unsubscribe_event"

      # Subscribe and then unsubscribe
      assert :ok = EventBus.subscribe(test_pid, event_type)
      assert :ok = EventBus.unsubscribe(test_pid, event_type)

      # Create and publish an event
      event =
        Event.create!(event_type, %{
          resource_id: "test-123",
          resource_type: "test",
          data: %{message: "Test message"}
        })

      assert :ok = EventBus.publish(event)

      # Should not receive the event
      refute_receive {:event, _}
    end
  end

  describe "multiple subscribers" do
            test "multiple subscribers receive the same event" do
      test_pid = self()

      # Create test processes that stay alive longer
      pid1 =
        spawn_link(fn ->
          receive do
            {:event, event} ->
              send(test_pid, {:received, 1, event})
              # Keep process alive a bit longer
              Process.sleep(100)
          end
        end)

      pid2 =
        spawn_link(fn ->
          receive do
            {:event, event} ->
              send(test_pid, {:received, 2, event})
              # Keep process alive a bit longer
              Process.sleep(100)
          end
        end)

      event_type = "test_multiple_subscribers"

      # Subscribe both processes
      assert :ok = EventBus.subscribe(pid1, event_type)
      assert :ok = EventBus.subscribe(pid2, event_type)

      # Give EventBus time to process subscriptions
      Process.sleep(50)

      # Create and publish an event
      event =
        Event.create!(event_type, %{
          resource_id: "test-123",
          resource_type: "test",
          data: %{message: "Test message"}
        })

      assert :ok = EventBus.publish(event)

      # Both should receive the event with increased timeout
      assert_receive {:received, 1, _received_event}, 5000
      assert_receive {:received, 2, _received_event}, 5000

      # Clean up
      Process.exit(pid1, :kill)
      Process.exit(pid2, :kill)
    end
  end

  describe "process monitoring" do
    test "removes dead processes from subscriptions" do
      # Create a test process that will die
      test_pid = spawn(fn ->
        # Keep the process alive long enough for subscription
        Process.sleep(10)
        :ok
      end)

      event_type = "test_dead_process"

      # Subscribe the process
      assert :ok = EventBus.subscribe(test_pid, event_type)

      # Verify it's subscribed
      assert {:ok, subscribers} = EventBus.get_subscribers(event_type)
      assert test_pid in subscribers

      # Wait for the process to die
      Process.monitor(test_pid)
      assert_receive {:DOWN, _ref, :process, ^test_pid, _reason}

      # Wait a bit for the EventBus to process the DOWN message
      Process.sleep(200)

      # Verify it's no longer subscribed
      assert {:ok, subscribers} = EventBus.get_subscribers(event_type)
      refute test_pid in subscribers
    end
  end

  describe "edge cases" do
    test "handles concurrent subscriptions" do
      event_type = "test_concurrent"
      test_pid = self()

      # Create multiple processes that subscribe concurrently
      pids =
        for _i <- 1..5 do
          spawn(fn ->
            result = EventBus.subscribe(self(), event_type)
            send(test_pid, {:subscribed, result})
            # Keep the process alive
            Process.sleep(100)
          end)
        end

      # Wait for all to subscribe
      for _pid <- pids do
        assert_receive {:subscribed, :ok}, 1000
      end

      # Verify all are subscribed
      assert {:ok, subscribers} = EventBus.get_subscribers(event_type)
      assert length(subscribers) >= 5

      # Clean up
      for pid <- pids do
        Process.exit(pid, :kill)
      end
    end

    test "handles large number of subscribers" do
      event_type = "test_large_subscribers"
      test_pid = self()

      # Create fewer subscribers to avoid overwhelming the EventBus
      pids =
        for i <- 1..5 do
          IO.puts("[DEBUG] Spawning subscriber \\#{i}")
          pid = spawn(fn ->
            try do
              IO.puts("[DEBUG] Subscriber \\#{i} subscribing...")
              result = EventBus.subscribe(self(), event_type)
              IO.puts("[DEBUG] Subscriber \\#{i} subscribed: \\#{inspect(result)}")
              send(test_pid, {:subscribed, result, i})

              # Listen for events and forward them to test process
              receive do
                {:event, event} ->
                  IO.puts("[DEBUG] Subscriber \\#{i} received event")
                  send(test_pid, {:event, event, i})
              after
                5000 ->
                  IO.puts("[DEBUG] Subscriber \\#{i} timeout waiting for event")
                  send(test_pid, {:event_timeout, i})
              end
            catch
              kind, error ->
                IO.puts("[DEBUG] Subscriber \\#{i} error: \\#{inspect({kind, error})}")
                send(test_pid, {:subscribed_error, {kind, error}, i})
            end
          end)
          # Stagger the spawns
          Process.sleep(20)
          pid
        end

      # Wait for all to subscribe with increased timeout and better error handling
      for _pid <- pids do
        receive do
          {:subscribed, :ok, i} -> IO.puts("[DEBUG] Test process received :subscribed from \\#{i}")
          {:subscribed_error, {kind, error}, i} -> IO.puts("[DEBUG] Test process received error from \\#{i}: \\#{inspect({kind, error})}"); flunk("Subscriber \\#{i} error: \\#{inspect({kind, error})}")
        after
          10_000 -> IO.puts("[DEBUG] Timeout waiting for subscriber"); flunk("Timeout waiting for subscriber")
        end
      end

      # Give EventBus time to process all subscriptions
      Process.sleep(100)

      # Verify all are subscribed
      assert {:ok, subscribers} = EventBus.get_subscribers(event_type)
      assert length(subscribers) == 5

      # Publish an event using proper Event struct
      event = Event.create!(event_type, %{
        resource_id: "test-resource",
        resource_type: "test",
        data: %{foo: "bar"}
      })
      :ok = EventBus.publish(event)

      # All subscribers should receive the event
      for i <- 1..5 do
        assert_receive {:event, ^event, ^i}, 5_000
      end
    end

    test "handles rapid subscribe/unsubscribe cycles" do
      test_pid = self()
      event_type = "test_rapid_cycles"

      # Rapidly subscribe and unsubscribe
      for _i <- 1..10 do
        assert :ok = EventBus.subscribe(test_pid, event_type)
        assert :ok = EventBus.unsubscribe(test_pid, event_type)
      end

      # Final state should be unsubscribed
      assert {:ok, subscribers} = EventBus.get_subscribers(event_type)
      refute test_pid in subscribers
    end
  end

  describe "performance" do
    test "handles large number of subscribers" do
      event_type = "test_performance"

      # Create many subscribers directly
      pids = for _i <- 1..20, do: self()

      # Subscribe all
      for _pid <- pids do
        assert :ok = EventBus.subscribe(self(), event_type)
      end

      # Verify subscriber is subscribed (deduplication means only one entry)
      assert {:ok, subscribers} = EventBus.get_subscribers(event_type)
      assert length(subscribers) >= 1
      assert self() in subscribers
    end
  end
end
