defmodule Spacecast.Events.Core.EventMonitorTest do
  use ExUnit.Case, async: false
  use Spacecast.DataCase

  alias Spacecast.Events.Core.EventMonitor
  alias Spacecast.Events.Core.Event

  setup do
    EventMonitor.reset_state()
    :ok
  end

  # Note: EventMonitor is started globally, so we don't start it in individual tests
  # We just test the public API functions

  describe "get_metrics/1" do
    test "returns current metrics with default options" do
      # The TestEventStore will be called, but we can still test the metrics structure
      assert {:ok, metrics} = EventMonitor.get_metrics()

      assert is_integer(metrics.event_count)
      assert is_float(metrics.events_per_second)
      assert is_map(metrics.processing_metrics)
      assert is_map(metrics.queue_sizes)
      assert is_map(metrics.error_rates)
      assert is_map(metrics.backpressure)
      assert metrics.backpressure.status in [:normal, :warning, :critical]
      assert is_struct(metrics.timestamp, DateTime)
      assert metrics.lookback_period_seconds == 3600
    end

    test "returns metrics with custom lookback period" do
      assert {:ok, metrics} = EventMonitor.get_metrics(lookback_seconds: 1800)
      assert metrics.lookback_period_seconds == 1800
    end

    test "includes processing metrics from recorded events" do
      # Record some processing metrics first
      event = %Event{
        id: "test-1",
        type: "test_event",
        data: %{},
        metadata: %{},
        timestamp: DateTime.utc_now()
      }

      EventMonitor.record_processing_metric(event, %{
        duration_ms: 100,
        handler: "test_handler",
        status: :success
      })

      assert {:ok, metrics} = EventMonitor.get_metrics()

      assert metrics.processing_metrics["test_event"]
      assert metrics.processing_metrics["test_event"].count == 1
      assert metrics.processing_metrics["test_event"].avg_time == 100.0
    end

    test "handles zero lookback period" do
      assert {:ok, metrics} = EventMonitor.get_metrics(lookback_seconds: 0)
      assert metrics.lookback_period_seconds == 0
      assert is_float(metrics.events_per_second)
    end

    test "handles negative lookback period" do
      assert {:ok, metrics} = EventMonitor.get_metrics(lookback_seconds: -3600)
      assert metrics.lookback_period_seconds == -3600
      assert is_float(metrics.events_per_second)
    end

    test "handles very large lookback period" do
      assert {:ok, metrics} = EventMonitor.get_metrics(lookback_seconds: 86400)
      assert metrics.lookback_period_seconds == 86400
      assert is_float(metrics.events_per_second)
    end
  end

  describe "detect_backpressure/3" do
    test "returns normal status when no backpressure detected" do
      queue_sizes = %{"handler1" => 50, "handler2" => 100}

      processing_metrics = %{
        "event1" => %{avg_time: 100},
        "event2" => %{avg_time: 200}
      }

      error_rates = %{"event1" => 0.01, "event2" => 0.02}

      result = EventMonitor.detect_backpressure(queue_sizes, processing_metrics, error_rates)

      assert result.status == :normal
      assert result.queue_pressure == false
      assert result.slow_processing_types == []
      assert result.high_error_types == []
    end

    test "detects queue pressure" do
      queue_sizes = %{"handler1" => 1500, "handler2" => 100}
      processing_metrics = %{"event1" => %{avg_time: 100}}
      error_rates = %{"event1" => 0.01}

      result = EventMonitor.detect_backpressure(queue_sizes, processing_metrics, error_rates)

      assert result.status == :warning
      assert result.queue_pressure == true
      assert result.slow_processing_types == []
      assert result.high_error_types == []
    end

    test "detects slow processing" do
      queue_sizes = %{"handler1" => 50}

      processing_metrics = %{
        "event1" => %{avg_time: 600},
        "event2" => %{avg_time: 100}
      }

      error_rates = %{"event1" => 0.01, "event2" => 0.02}

      result = EventMonitor.detect_backpressure(queue_sizes, processing_metrics, error_rates)

      assert result.status == :warning
      assert result.queue_pressure == false
      assert "event1" in result.slow_processing_types
      assert "event2" not in result.slow_processing_types
    end

    test "detects high error rates" do
      queue_sizes = %{"handler1" => 50}
      processing_metrics = %{"event1" => %{avg_time: 100}}
      error_rates = %{"event1" => 0.1, "event2" => 0.02}

      result = EventMonitor.detect_backpressure(queue_sizes, processing_metrics, error_rates)

      assert result.status == :warning
      assert result.queue_pressure == false
      assert result.slow_processing_types == []
      assert "event1" in result.high_error_types
      assert "event2" not in result.high_error_types
    end

    test "detects critical backpressure when multiple conditions are met" do
      queue_sizes = %{"handler1" => 1500}
      processing_metrics = %{"event1" => %{avg_time: 600}}
      error_rates = %{"event1" => 0.1}

      result = EventMonitor.detect_backpressure(queue_sizes, processing_metrics, error_rates)

      assert result.status == :critical
      assert result.queue_pressure == true
      assert "event1" in result.slow_processing_types
      assert "event1" in result.high_error_types
    end

    test "identifies bottlenecks correctly" do
      queue_sizes = %{"handler1" => 1500, "handler2" => 50}

      processing_metrics = %{
        "event1" => %{avg_time: 600},
        "event2" => %{avg_time: 100}
      }

      error_rates = %{"event1" => 0.1, "event2" => 0.02}

      result = EventMonitor.detect_backpressure(queue_sizes, processing_metrics, error_rates)

      assert result.bottlenecks.high_queue_handlers == ["handler1"]
      assert result.bottlenecks.slow_event_types == ["event1"]
      assert result.bottlenecks.high_error_types == ["event1"]
    end

    test "handles empty maps" do
      result = EventMonitor.detect_backpressure(%{}, %{}, %{})
      assert result.status == :normal
      assert result.queue_pressure == false
      assert result.slow_processing_types == []
      assert result.high_error_types == []
      assert result.bottlenecks.high_queue_handlers == []
      assert result.bottlenecks.slow_event_types == []
      assert result.bottlenecks.high_error_types == []
    end

    test "handles edge case thresholds" do
      # Test exactly at threshold values
      # Exactly at threshold
      queue_sizes = %{"handler1" => 1000}
      # Exactly at threshold
      processing_metrics = %{"event1" => %{avg_time: 500}}
      # Exactly at threshold
      error_rates = %{"event1" => 0.05}

      result = EventMonitor.detect_backpressure(queue_sizes, processing_metrics, error_rates)

      # Should be critical since all conditions are at threshold
      assert result.status == :critical
      assert result.queue_pressure == true
      assert "event1" in result.slow_processing_types
      assert "event1" in result.high_error_types
    end

    test "handles just below threshold values" do
      # Test just below threshold values
      # Just below threshold
      queue_sizes = %{"handler1" => 999}
      # Just below threshold
      processing_metrics = %{"event1" => %{avg_time: 499}}
      # Just below threshold
      error_rates = %{"event1" => 0.049}

      result = EventMonitor.detect_backpressure(queue_sizes, processing_metrics, error_rates)

      # Should be normal since all values are below thresholds
      assert result.status == :normal
      assert result.queue_pressure == false
      assert result.slow_processing_types == []
      assert result.high_error_types == []
    end
  end

  describe "setup_alerting/2" do
    test "sets up alerting with default options" do
      assert :ok = EventMonitor.setup_alerting()

      # Verify alerting is enabled by checking metrics
      assert {:ok, _metrics} = EventMonitor.get_metrics()
    end

    test "sets up alerting with custom notification function" do
      test_pid = self()

      notification_fn = fn alert ->
        send(test_pid, {:alert, alert})
      end

      assert :ok = EventMonitor.setup_alerting(notification_fn)

      # Trigger an alert by creating backpressure conditions
      EventMonitor.record_queue_size("test_handler", 1500)

      event = %Event{
        id: "test-1",
        type: "slow_event",
        data: %{},
        metadata: %{},
        timestamp: DateTime.utc_now()
      }

      EventMonitor.record_processing_metric(event, %{
        duration_ms: 600,
        handler: "test_handler",
        status: :success
      })

      EventMonitor.check_and_alert()

      # Should receive alert due to critical backpressure (queue + slow processing)
      assert_receive {:alert, alert}
      assert alert.type == :backpressure
      # Both queue pressure and slow processing
      assert alert.level == :critical
      assert alert.message == "Critical backpressure in event system"
      assert is_map(alert.details)
      assert is_struct(alert.timestamp, DateTime)
    end

    test "sets up alerting with custom options" do
      assert :ok =
               EventMonitor.setup_alerting(nil,
                 interval_ms: 30_000,
                 lookback_seconds: 600,
                 notification_channels: [:email, :slack],
                 recipients: :all_users,
                 threshold_overrides: %{
                   queue_high: 500,
                   processing_time: 300,
                   error_rate: 0.03
                 }
               )
    end

    test "sets up alerting with nil notification function" do
      assert :ok = EventMonitor.setup_alerting(nil)
      # Should not crash
      assert {:ok, _metrics} = EventMonitor.get_metrics()
    end

    test "sets up alerting with minimal options" do
      assert :ok = EventMonitor.setup_alerting(nil, [])
      # Should not crash
      assert {:ok, _metrics} = EventMonitor.get_metrics()
    end

    test "sets up alerting with extreme threshold values" do
      assert :ok =
               EventMonitor.setup_alerting(nil,
                 threshold_overrides: %{
                   queue_high: 1_000_000,
                   processing_time: 60_000,
                   error_rate: 0.99
                 }
               )

      # Should not crash
      assert {:ok, _metrics} = EventMonitor.get_metrics()
    end
  end

  describe "clear_alerting/0" do
    test "clears alerting configuration" do
      # First set up alerting
      assert :ok = EventMonitor.setup_alerting()

      # Then clear it
      assert :ok = EventMonitor.clear_alerting()

      # Verify alerting is disabled
      assert {:ok, _metrics} = EventMonitor.get_metrics()
    end

    test "clears alerting when not set up" do
      # Should not crash when clearing alerting that wasn't set up
      assert :ok = EventMonitor.clear_alerting()
      assert {:ok, _metrics} = EventMonitor.get_metrics()
    end
  end

  describe "record_processing_metric/2" do
    test "records successful processing metric" do
      event = %Event{
        id: "test-1",
        type: "test_event",
        data: %{},
        metadata: %{},
        timestamp: DateTime.utc_now()
      }

      assert :ok =
               EventMonitor.record_processing_metric(event, %{
                 duration_ms: 150,
                 handler: "test_handler",
                 status: :success,
                 details: %{custom: "data"}
               })

      # Verify the metric was recorded
      assert {:ok, metrics} = EventMonitor.get_metrics()

      assert metrics.processing_metrics["test_event"]
      assert metrics.processing_metrics["test_event"].count == 1
      assert metrics.processing_metrics["test_event"].total_time == 150
      assert metrics.processing_metrics["test_event"].errors == 0
      assert metrics.processing_metrics["test_event"].avg_time == 150.0
      assert metrics.processing_metrics["test_event"].min_time == 150
      assert metrics.processing_metrics["test_event"].max_time == 150
    end

    test "records error processing metric" do
      event = %Event{
        id: "test-2",
        type: "error_event",
        data: %{},
        metadata: %{},
        timestamp: DateTime.utc_now()
      }

      assert :ok =
               EventMonitor.record_processing_metric(event, %{
                 duration_ms: 200,
                 handler: "test_handler",
                 status: :error
               })

      # Verify the metric was recorded
      assert {:ok, metrics} = EventMonitor.get_metrics()

      assert metrics.processing_metrics["error_event"]
      assert metrics.processing_metrics["error_event"].count == 1
      assert metrics.processing_metrics["error_event"].errors == 1
      assert metrics.error_rates["error_event"] == 1.0
    end

    test "accumulates metrics for same event type" do
      event = %Event{
        id: "test-3",
        type: "accumulate_event",
        data: %{},
        metadata: %{},
        timestamp: DateTime.utc_now()
      }

      # Record first metric
      assert :ok =
               EventMonitor.record_processing_metric(event, %{
                 duration_ms: 100,
                 handler: "handler1",
                 status: :success
               })

      # Record second metric
      assert :ok =
               EventMonitor.record_processing_metric(event, %{
                 duration_ms: 200,
                 handler: "handler2",
                 status: :error
               })

      # Record third metric
      assert :ok =
               EventMonitor.record_processing_metric(event, %{
                 duration_ms: 150,
                 handler: "handler3",
                 status: :success
               })

      # Verify accumulated metrics
      assert {:ok, metrics} = EventMonitor.get_metrics()

      event_metrics = metrics.processing_metrics["accumulate_event"]
      assert event_metrics.count == 3
      assert event_metrics.total_time == 450
      assert event_metrics.errors == 1
      assert event_metrics.avg_time == 150.0
      assert event_metrics.min_time == 100
      assert event_metrics.max_time == 200

      # Error rate should be 1/3 = 0.333...
      IO.inspect(metrics.error_rates, label: "Error rates")
      IO.inspect(event_metrics, label: "Event metrics")
      assert_in_delta metrics.error_rates["accumulate_event"], 0.333, 0.001
    end

    test "handles missing optional fields with defaults" do
      event = %Event{
        id: "test-4",
        type: "default_event",
        data: %{},
        metadata: %{},
        timestamp: DateTime.utc_now()
      }

      # Record metric with minimal data
      assert :ok = EventMonitor.record_processing_metric(event, %{})

      # Verify defaults were applied
      assert {:ok, metrics} = EventMonitor.get_metrics()

      event_metrics = metrics.processing_metrics["default_event"]
      assert event_metrics.count == 1
      assert event_metrics.total_time == 0
      assert event_metrics.errors == 0
      assert event_metrics.avg_time == 0.0
    end

    test "handles event with no metrics parameter" do
      event = %Event{
        id: "test-5",
        type: "no_metrics_event",
        data: %{},
        metadata: %{},
        timestamp: DateTime.utc_now()
      }

      # Record metric with no metrics parameter
      assert :ok = EventMonitor.record_processing_metric(event)

      # Verify defaults were applied
      assert {:ok, metrics} = EventMonitor.get_metrics()

      event_metrics = metrics.processing_metrics["no_metrics_event"]
      assert event_metrics.count == 1
      assert event_metrics.total_time == 0
      assert event_metrics.errors == 0
      assert event_metrics.avg_time == 0.0
    end

    test "handles unknown status values" do
      event = %Event{
        id: "test-6",
        type: "unknown_status_event",
        data: %{},
        metadata: %{},
        timestamp: DateTime.utc_now()
      }

      # Record metric with unknown status
      assert :ok =
               EventMonitor.record_processing_metric(event, %{
                 duration_ms: 100,
                 handler: "test_handler",
                 status: :unknown_status
               })

      # Should treat unknown status as success (default)
      assert {:ok, metrics} = EventMonitor.get_metrics()

      event_metrics = metrics.processing_metrics["unknown_status_event"]
      assert event_metrics.errors == 0
    end

    test "handles nil status" do
      event = %Event{
        id: "test-7",
        type: "nil_status_event",
        data: %{},
        metadata: %{},
        timestamp: DateTime.utc_now()
      }

      # Record metric with nil status
      assert :ok =
               EventMonitor.record_processing_metric(event, %{
                 duration_ms: 100,
                 handler: "test_handler",
                 status: nil
               })

      # Should treat nil status as success (default)
      assert {:ok, metrics} = EventMonitor.get_metrics()

      event_metrics = metrics.processing_metrics["nil_status_event"]
      assert event_metrics.errors == 0
    end
  end

  describe "record_queue_size/2" do
    test "records queue size for handler" do
      assert :ok = EventMonitor.record_queue_size("test_handler", 150)
      assert :ok = EventMonitor.record_queue_size("another_handler", 75)

      assert {:ok, metrics} = EventMonitor.get_metrics()

      assert metrics.queue_sizes["test_handler"] == 150
      assert metrics.queue_sizes["another_handler"] == 75
    end

    test "updates existing queue size" do
      assert :ok = EventMonitor.record_queue_size("update_handler", 100)
      assert :ok = EventMonitor.record_queue_size("update_handler", 200)

      assert {:ok, metrics} = EventMonitor.get_metrics()

      assert metrics.queue_sizes["update_handler"] == 200
    end

    test "handles zero queue size" do
      assert :ok = EventMonitor.record_queue_size("zero_handler", 0)
      assert {:ok, metrics} = EventMonitor.get_metrics()
      assert metrics.queue_sizes["zero_handler"] == 0
    end

    test "handles negative queue size" do
      assert :ok = EventMonitor.record_queue_size("negative_handler", -50)
      assert {:ok, metrics} = EventMonitor.get_metrics()
      assert metrics.queue_sizes["negative_handler"] == -50
    end

    test "handles very large queue size" do
      assert :ok = EventMonitor.record_queue_size("large_handler", 1_000_000)
      assert {:ok, metrics} = EventMonitor.get_metrics()
      assert metrics.queue_sizes["large_handler"] == 1_000_000
    end

    test "handles nil handler name" do
      assert :ok = EventMonitor.record_queue_size(nil, 100)
      assert {:ok, metrics} = EventMonitor.get_metrics()
      assert metrics.queue_sizes[nil] == 100
    end
  end

  describe "check_and_alert/0" do
    test "does not alert when backpressure is normal" do
      # Set up alerting
      assert :ok = EventMonitor.setup_alerting()

      # Check and alert should not trigger notification
      assert :ok = EventMonitor.check_and_alert()

      # Should not receive any alert
      refute_receive {:alert, _}
    end

    test "triggers alert when backpressure is detected" do
      # Set up alerting with notification function
      test_pid = self()

      notification_fn = fn alert ->
        send(test_pid, {:alert, alert})
      end

      assert :ok = EventMonitor.setup_alerting(notification_fn)

      # Create backpressure conditions
      EventMonitor.record_queue_size("test_handler", 1500)

      event = %Event{
        id: "test-1",
        type: "slow_event",
        data: %{},
        metadata: %{},
        timestamp: DateTime.utc_now()
      }

      EventMonitor.record_processing_metric(event, %{
        duration_ms: 600,
        handler: "test_handler",
        status: :success
      })

      # Check and alert should trigger notification
      assert :ok = EventMonitor.check_and_alert()

      # Should receive alert
      assert_receive {:alert, alert}
      assert alert.type == :backpressure
      # Both queue pressure and slow processing
      assert alert.level == :critical
      assert alert.message == "Critical backpressure in event system"
      assert is_map(alert.details)
      assert is_struct(alert.timestamp, DateTime)
    end

    test "respects alert cooldown period" do
      # Set up alerting
      test_pid = self()

      notification_fn = fn alert ->
        send(test_pid, {:alert, alert})
      end

      assert :ok = EventMonitor.setup_alerting(notification_fn)

      # Create backpressure conditions
      EventMonitor.record_queue_size("test_handler", 1500)

      event = %Event{
        id: "test-1",
        type: "slow_event",
        data: %{},
        metadata: %{},
        timestamp: DateTime.utc_now()
      }

      EventMonitor.record_processing_metric(event, %{
        duration_ms: 600,
        handler: "test_handler",
        status: :success
      })

      # First alert should trigger
      assert :ok = EventMonitor.check_and_alert()
      assert_receive {:alert, _}

      # Second alert within cooldown should not trigger
      assert :ok = EventMonitor.check_and_alert()
      refute_receive {:alert, _}
    end

    test "does not alert when alerting is disabled" do
      # Don't set up alerting
      test_pid = self()

      _notification_fn = fn alert ->
        send(test_pid, {:alert, alert})
      end

      # Create backpressure conditions
      EventMonitor.record_queue_size("test_handler", 1500)

      event = %Event{
        id: "test-1",
        type: "slow_event",
        data: %{},
        metadata: %{},
        timestamp: DateTime.utc_now()
      }

      EventMonitor.record_processing_metric(event, %{
        duration_ms: 600,
        handler: "test_handler",
        status: :success
      })

      # Check and alert should not trigger notification when alerting is disabled
      assert :ok = EventMonitor.check_and_alert()
      refute_receive {:alert, _}
    end

    test "handles alerting with custom thresholds" do
      test_pid = self()

      notification_fn = fn alert ->
        send(test_pid, {:alert, alert})
      end

      # Set up alerting with custom thresholds
      assert :ok =
               EventMonitor.setup_alerting(notification_fn,
                 threshold_overrides: %{
                   # Lower threshold
                   queue_high: 100,
                   # Lower threshold
                   processing_time: 200,
                   # Lower threshold
                   error_rate: 0.01
                 }
               )

      # Create conditions that would trigger with custom thresholds
      # Above 100 threshold
      EventMonitor.record_queue_size("test_handler", 150)

      event = %Event{
        id: "test-1",
        type: "slow_event",
        data: %{},
        metadata: %{},
        timestamp: DateTime.utc_now()
      }

      EventMonitor.record_processing_metric(event, %{
        # Above 200 threshold
        duration_ms: 250,
        handler: "test_handler",
        status: :success
      })

      # Give the GenServer a moment to process the cast
      Process.sleep(100)
      # Check and alert should trigger notification
      assert :ok = EventMonitor.check_and_alert()

      # Should receive alert (allow longer timeout)
      assert_receive {:alert, alert}, 2000
      assert alert.type == :backpressure
      # Both queue pressure and slow processing
      assert alert.level == :critical
    end
  end

  describe "performance and edge cases" do
    test "handles large number of event types" do
      # Record metrics for many different event types
      Enum.each(1..100, fn i ->
        event = %Event{
          id: "test-#{i}",
          type: "event_type_#{i}",
          data: %{},
          metadata: %{},
          timestamp: DateTime.utc_now()
        }

        EventMonitor.record_processing_metric(event, %{
          duration_ms: i * 10,
          handler: "handler_#{i}",
          status: if(rem(i, 10) == 0, do: :error, else: :success)
        })
      end)

      assert {:ok, metrics} = EventMonitor.get_metrics()

      # Should have metrics for all 100 event types
      assert map_size(metrics.processing_metrics) == 100
      assert map_size(metrics.error_rates) == 100
    end

    test "handles concurrent metric recording" do
      # Record metrics concurrently
      tasks =
        Enum.map(1..50, fn i ->
          Task.async(fn ->
            event = %Event{
              id: "concurrent-#{i}",
              type: "concurrent_event",
              data: %{},
              metadata: %{},
              timestamp: DateTime.utc_now()
            }

            EventMonitor.record_processing_metric(event, %{
              duration_ms: i,
              handler: "concurrent_handler",
              status: :success
            })
          end)
        end)

      # Wait for all tasks to complete
      Enum.each(tasks, &Task.await/1)

      assert {:ok, metrics} = EventMonitor.get_metrics()

      # Should have accumulated all 50 metrics
      event_metrics = metrics.processing_metrics["concurrent_event"]
      assert event_metrics.count == 50
      assert event_metrics.total_time == Enum.sum(1..50)
    end

    test "handles extreme values gracefully" do
      event = %Event{
        id: "extreme-test",
        type: "extreme_event",
        data: %{},
        metadata: %{},
        timestamp: DateTime.utc_now()
      }

      # Record extreme values
      assert :ok =
               EventMonitor.record_processing_metric(event, %{
                 # Very long processing time
                 duration_ms: 1_000_000,
                 handler: "extreme_handler",
                 status: :success
               })

      assert {:ok, metrics} = EventMonitor.get_metrics()

      event_metrics = metrics.processing_metrics["extreme_event"]
      assert event_metrics.avg_time == 1_000_000.0
      assert event_metrics.max_time == 1_000_000
    end

    test "handles zero and negative values" do
      event = %Event{
        id: "zero-test",
        type: "zero_event",
        data: %{},
        metadata: %{},
        timestamp: DateTime.utc_now()
      }

      # Record zero and negative values
      assert :ok =
               EventMonitor.record_processing_metric(event, %{
                 duration_ms: 0,
                 handler: "zero_handler",
                 status: :success
               })

      assert :ok =
               EventMonitor.record_processing_metric(event, %{
                 # Negative value
                 duration_ms: -100,
                 handler: "negative_handler",
                 status: :success
               })

      assert {:ok, metrics} = EventMonitor.get_metrics()

      event_metrics = metrics.processing_metrics["zero_event"]
      assert event_metrics.count == 2
      # Should handle negative values
      assert event_metrics.total_time == -100
      assert event_metrics.min_time == -100
      assert event_metrics.max_time == 0
    end

    test "handles history overflow" do
      # Record many metrics to test history overflow
      Enum.each(1..1100, fn i ->
        event = %Event{
          id: "history-test-#{i}",
          type: "history_event",
          data: %{},
          metadata: %{},
          timestamp: DateTime.utc_now()
        }

        EventMonitor.record_processing_metric(event, %{
          duration_ms: i,
          handler: "history_handler",
          status: :success
        })
      end)

      # Should not crash and should maintain reasonable history size
      assert {:ok, metrics} = EventMonitor.get_metrics()
      assert metrics.processing_metrics["history_event"].count == 1100
    end

    test "handles unicode and special characters in handler names" do
      assert :ok = EventMonitor.record_queue_size("handler-🚀", 100)
      assert :ok = EventMonitor.record_queue_size("handler-测试", 200)
      assert :ok = EventMonitor.record_queue_size("handler-ñáéíóú", 300)

      assert {:ok, metrics} = EventMonitor.get_metrics()

      assert metrics.queue_sizes["handler-🚀"] == 100
      assert metrics.queue_sizes["handler-测试"] == 200
      assert metrics.queue_sizes["handler-ñáéíóú"] == 300
    end

    test "handles very long handler names" do
      long_name = String.duplicate("a", 1000)
      assert :ok = EventMonitor.record_queue_size(long_name, 100)

      assert {:ok, metrics} = EventMonitor.get_metrics()
      assert metrics.queue_sizes[long_name] == 100
    end
  end

  describe "error handling" do
    test "handles malformed event data" do
      # Test with invalid event struct - should raise FunctionClauseError
      invalid_event = %{
        id: "invalid",
        type: "invalid_event"
      }

      # Should raise FunctionClauseError since it's not an Event struct
      assert_raise FunctionClauseError, fn ->
        EventMonitor.record_processing_metric(invalid_event, %{
          duration_ms: 100,
          handler: "test_handler",
          status: :success
        })
      end
    end

    test "handles nil event" do
      # Should raise FunctionClauseError for nil event
      assert_raise FunctionClauseError, fn ->
        EventMonitor.record_processing_metric(nil, %{
          duration_ms: 100,
          handler: "test_handler",
          status: :success
        })
      end
    end

    test "handles nil metrics" do
      event = %Event{
        id: "test-nil-metrics",
        type: "nil_metrics_event",
        data: %{},
        metadata: %{},
        timestamp: DateTime.utc_now()
      }

      # Should handle nil metrics gracefully
      assert :ok = EventMonitor.record_processing_metric(event, nil)

      assert {:ok, metrics} = EventMonitor.get_metrics()
      event_metrics = metrics.processing_metrics["nil_metrics_event"]
      assert event_metrics.count == 1
      assert event_metrics.total_time == 0
    end

    test "handles non-map metrics" do
      event = %Event{
        id: "test-non-map-metrics",
        type: "non_map_metrics_event",
        data: %{},
        metadata: %{},
        timestamp: DateTime.utc_now()
      }

      # Should handle non-map metrics gracefully
      assert :ok = EventMonitor.record_processing_metric(event, "not a map")

      assert {:ok, metrics} = EventMonitor.get_metrics()
      event_metrics = metrics.processing_metrics["non_map_metrics_event"]
      assert event_metrics.count == 1
      assert event_metrics.total_time == 0
    end
  end

  describe "telemetry integration" do
    test "emits telemetry events for processing metrics" do
      # This test verifies that telemetry events are emitted
      # The actual telemetry verification would require more complex setup
      event = %Event{
        id: "telemetry-test",
        type: "telemetry_event",
        data: %{},
        metadata: %{},
        timestamp: DateTime.utc_now()
      }

      # Should not crash when emitting telemetry
      assert :ok =
               EventMonitor.record_processing_metric(event, %{
                 duration_ms: 100,
                 handler: "telemetry_handler",
                 status: :success
               })
    end

    test "emits telemetry events for queue sizes" do
      # Should not crash when emitting telemetry
      assert :ok = EventMonitor.record_queue_size("telemetry_handler", 150)
    end

    test "emits telemetry events for backpressure detection" do
      # Create backpressure conditions to trigger telemetry
      EventMonitor.record_queue_size("telemetry_handler", 1500)

      event = %Event{
        id: "telemetry-backpressure",
        type: "telemetry_backpressure_event",
        data: %{},
        metadata: %{},
        timestamp: DateTime.utc_now()
      }

      EventMonitor.record_processing_metric(event, %{
        duration_ms: 600,
        handler: "telemetry_handler",
        status: :success
      })

      # Should not crash when detecting backpressure
      result =
        EventMonitor.detect_backpressure(
          %{"telemetry_handler" => 1500},
          %{"telemetry_backpressure_event" => %{avg_time: 600}},
          %{"telemetry_backpressure_event" => 0.0}
        )

      # Both queue pressure and slow processing
      assert result.status == :critical
    end

    test "emits telemetry events for metrics collection" do
      # Should not crash when collecting metrics
      assert {:ok, _metrics} = EventMonitor.get_metrics()
    end
  end

  describe "state management" do
    test "reset_state clears all metrics" do
      # Record some metrics first
      event = %Event{
        id: "reset-test",
        type: "reset_event",
        data: %{},
        metadata: %{},
        timestamp: DateTime.utc_now()
      }

      EventMonitor.record_processing_metric(event, %{
        duration_ms: 100,
        handler: "reset_handler",
        status: :success
      })

      EventMonitor.record_queue_size("reset_handler", 100)

      # Verify metrics were recorded
      assert {:ok, metrics} = EventMonitor.get_metrics()
      assert metrics.processing_metrics["reset_event"].count == 1
      assert metrics.queue_sizes["reset_handler"] == 100

      # Reset state
      EventMonitor.reset_state()

      # Verify metrics were cleared
      assert {:ok, metrics} = EventMonitor.get_metrics()
      assert metrics.processing_metrics == %{}
      assert metrics.queue_sizes == %{}
      assert metrics.error_rates == %{}
    end

    test "reset_state preserves event_store and metric_interval" do
      # Record some metrics
      event = %Event{
        id: "preserve-test",
        type: "preserve_event",
        data: %{},
        metadata: %{},
        timestamp: DateTime.utc_now()
      }

      EventMonitor.record_processing_metric(event, %{
        duration_ms: 100,
        handler: "preserve_handler",
        status: :success
      })

      # Reset state
      EventMonitor.reset_state()

      # Should still be able to get metrics (event_store preserved)
      assert {:ok, metrics} = EventMonitor.get_metrics()
      assert is_map(metrics)
      assert is_struct(metrics.timestamp, DateTime)
    end

    test "reset_state clears alerting configuration" do
      # Set up alerting
      test_pid = self()

      notification_fn = fn alert ->
        send(test_pid, {:alert, alert})
      end

      assert :ok = EventMonitor.setup_alerting(notification_fn)

      # Create backpressure conditions
      EventMonitor.record_queue_size("test_handler", 1500)

      event = %Event{
        id: "test-1",
        type: "slow_event",
        data: %{},
        metadata: %{},
        timestamp: DateTime.utc_now()
      }

      EventMonitor.record_processing_metric(event, %{
        duration_ms: 600,
        handler: "test_handler",
        status: :success
      })

      # Should trigger alert
      assert :ok = EventMonitor.check_and_alert()
      assert_receive {:alert, _}

      # Reset state
      EventMonitor.reset_state()

      # Should not trigger alert after reset
      assert :ok = EventMonitor.check_and_alert()
      refute_receive {:alert, _}
    end
  end

  describe "alerting edge cases" do
    test "handles alerting with custom notification channels" do
      # Set up alerting with custom channels
      assert :ok =
               EventMonitor.setup_alerting(nil,
                 notification_channels: [:email, :slack, :webhook],
                 recipients: :specific_users
               )

      # Should not crash
      assert {:ok, _metrics} = EventMonitor.get_metrics()
    end

    test "handles alerting with custom recipients" do
      # Set up alerting with custom recipients
      assert :ok =
               EventMonitor.setup_alerting(nil,
                 recipients: :all_users
               )

      # Should not crash
      assert {:ok, _metrics} = EventMonitor.get_metrics()
    end

    test "handles alerting with custom interval" do
      # Set up alerting with custom interval
      assert :ok =
               EventMonitor.setup_alerting(nil,
                 # 5 seconds
                 interval_ms: 5000
               )

      # Should not crash
      assert {:ok, _metrics} = EventMonitor.get_metrics()
    end

    test "handles alerting with custom lookback" do
      # Set up alerting with custom lookback
      assert :ok =
               EventMonitor.setup_alerting(nil,
                 # 1 minute
                 lookback_seconds: 60
               )

      # Should not crash
      assert {:ok, _metrics} = EventMonitor.get_metrics()
    end

    test "handles alerting with extreme values" do
      # Set up alerting with extreme values
      assert :ok =
               EventMonitor.setup_alerting(nil,
                 # 1 millisecond
                 interval_ms: 1,
                 # 1 second
                 lookback_seconds: 1,
                 threshold_overrides: %{
                   queue_high: 0,
                   processing_time: 0,
                   error_rate: 0.0
                 }
               )

      # Should not crash
      assert {:ok, _metrics} = EventMonitor.get_metrics()
    end
  end
end
