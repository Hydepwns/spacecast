defmodule Spacecast.Security.DetectorTest do
  use ExUnit.Case, async: true
  alias Spacecast.Security.Detector

  describe "detect_suspicious_activity/1" do
    test "detects multiple failed logins above threshold" do
      activity = %{
        "type" => "multiple_failed_logins",
        "count" => 6,
        "user_id" => "123",
        "timestamp" => "2025-01-01T00:00:00Z"
      }

      assert {:warning, "Suspicious activity detected"} =
               Detector.detect_suspicious_activity(activity)
    end

    test "does not detect multiple failed logins below threshold" do
      activity = %{
        "type" => "multiple_failed_logins",
        "count" => 5,
        "user_id" => "123"
      }

      assert {:ok, "no-threat"} = Detector.detect_suspicious_activity(activity)
    end

    test "detects rapid requests above threshold" do
      activity = %{
        "type" => "rapid_requests",
        "count" => 101,
        "ip_address" => "192.168.1.1",
        "timeframe" => "1_minute"
      }

      assert {:warning, "Potential DDoS attack detected"} =
               Detector.detect_suspicious_activity(activity)
    end

    test "does not detect rapid requests below threshold" do
      activity = %{
        "type" => "rapid_requests",
        "count" => 100,
        "ip_address" => "192.168.1.1"
      }

      assert {:ok, "no-threat"} = Detector.detect_suspicious_activity(activity)
    end

    test "detects unusual access pattern" do
      activity = %{
        "type" => "unusual_access_pattern",
        "details" => %{
          "user_id" => "456",
          "location" => "unusual_country",
          "time" => "unusual_hour"
        }
      }

      assert {:warning, "Unusual access pattern detected"} =
               Detector.detect_suspicious_activity(activity)
    end

    test "handles unknown activity type" do
      activity = %{
        "type" => "unknown_activity",
        "count" => 10
      }

      assert {:ok, "no-threat"} = Detector.detect_suspicious_activity(activity)
    end

    test "returns error for invalid activity data" do
      assert {:error, "Invalid activity data"} =
               Detector.detect_suspicious_activity("invalid_data")

      assert {:error, "Invalid activity data"} = Detector.detect_suspicious_activity(123)
      assert {:error, "Invalid activity data"} = Detector.detect_suspicious_activity(nil)
    end
  end

  describe "activity patterns" do
    test "handles multiple failed logins with additional data" do
      activity = %{
        "type" => "multiple_failed_logins",
        "count" => 10,
        "user_id" => "789",
        "ip_address" => "10.0.0.1",
        "user_agent" => "Mozilla/5.0",
        "attempts" => [
          %{"timestamp" => "2025-01-01T00:00:00Z", "password" => "wrong1"},
          %{"timestamp" => "2025-01-01T00:01:00Z", "password" => "wrong2"}
        ]
      }

      assert {:warning, "Suspicious activity detected"} =
               Detector.detect_suspicious_activity(activity)
    end

    test "handles rapid requests with detailed information" do
      activity = %{
        "type" => "rapid_requests",
        "count" => 150,
        "ip_address" => "192.168.1.100",
        "timeframe" => "30_seconds",
        "endpoints" => ["/api/login", "/api/register", "/api/reset"],
        "user_agents" => ["bot1", "bot2", "bot3"]
      }

      assert {:warning, "Potential DDoS attack detected"} =
               Detector.detect_suspicious_activity(activity)
    end

    test "handles unusual access pattern with complex details" do
      activity = %{
        "type" => "unusual_access_pattern",
        "details" => %{
          "user_id" => "999",
          "previous_locations" => ["US", "CA"],
          "current_location" => "RU",
          "previous_times" => ["09:00", "17:00"],
          "current_time" => "03:00",
          "device_change" => true,
          "browser_change" => true
        }
      }

      assert {:warning, "Unusual access pattern detected"} =
               Detector.detect_suspicious_activity(activity)
    end
  end

  describe "edge cases" do
    test "handles boundary values for multiple failed logins" do
      activity_5 = %{"type" => "multiple_failed_logins", "count" => 5}
      activity_6 = %{"type" => "multiple_failed_logins", "count" => 6}

      assert {:ok, "no-threat"} = Detector.detect_suspicious_activity(activity_5)

      assert {:warning, "Suspicious activity detected"} =
               Detector.detect_suspicious_activity(activity_6)
    end

    test "handles boundary values for rapid requests" do
      activity_100 = %{"type" => "rapid_requests", "count" => 100}
      activity_101 = %{"type" => "rapid_requests", "count" => 101}

      assert {:ok, "no-threat"} = Detector.detect_suspicious_activity(activity_100)

      assert {:warning, "Potential DDoS attack detected"} =
               Detector.detect_suspicious_activity(activity_101)
    end

    test "handles very high counts" do
      activity = %{
        "type" => "multiple_failed_logins",
        "count" => 1_000_000
      }

      assert {:warning, "Suspicious activity detected"} =
               Detector.detect_suspicious_activity(activity)
    end

    test "handles empty details in unusual access pattern" do
      activity = %{
        "type" => "unusual_access_pattern",
        "details" => %{}
      }

      assert {:warning, "Unusual access pattern detected"} =
               Detector.detect_suspicious_activity(activity)
    end

    test "handles missing type field" do
      activity = %{"count" => 10, "user_id" => "123"}

      assert {:ok, "no-threat"} = Detector.detect_suspicious_activity(activity)
    end

    test "handles missing count field" do
      activity = %{"type" => "multiple_failed_logins", "user_id" => "123"}

      assert {:ok, "no-threat"} = Detector.detect_suspicious_activity(activity)
    end
  end

  describe "data validation" do
    test "handles non-integer count values" do
      activity = %{
        "type" => "multiple_failed_logins",
        "count" => "6"
      }

      assert {:warning, "Suspicious activity detected"} =
               Detector.detect_suspicious_activity(activity)
    end

    test "handles negative count values" do
      activity = %{
        "type" => "multiple_failed_logins",
        "count" => -5
      }

      assert {:ok, "no-threat"} = Detector.detect_suspicious_activity(activity)
    end

    test "handles float count values" do
      activity = %{
        "type" => "multiple_failed_logins",
        "count" => 5.5
      }

      assert {:warning, "Suspicious activity detected"} =
               Detector.detect_suspicious_activity(activity)
    end
  end
end
