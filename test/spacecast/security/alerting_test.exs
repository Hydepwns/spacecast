defmodule Spacecast.Security.AlertingTest do
  use ExUnit.Case, async: true
  alias Spacecast.Security.Alerting

  describe "send_alert/2" do
    test "sends security breach alert successfully" do
      details = %{
        "user_id" => "123",
        "ip_address" => "192.168.1.1",
        "timestamp" => "2025-01-01T00:00:00Z"
      }

      assert {:ok, "alert-sent"} = Alerting.send_alert("security_breach", details)
    end

    test "sends suspicious activity alert successfully" do
      details = %{
        "user_id" => "456",
        "activity_type" => "multiple_failed_logins",
        "count" => 10
      }

      assert {:ok, "alert-sent"} = Alerting.send_alert("suspicious_activity", details)
    end

    test "sends default alert for unknown alert type" do
      details = %{"message" => "Unknown threat detected"}

      assert {:ok, "alert-sent"} = Alerting.send_alert("unknown_threat", details)
    end

    test "returns error for invalid alert type" do
      details = %{"message" => "Test alert"}

      assert {:error, "Invalid parameters"} = Alerting.send_alert(123, details)
    end

    test "returns error for invalid details" do
      assert {:error, "Invalid parameters"} =
               Alerting.send_alert("security_breach", "invalid_details")
    end

    test "returns error for nil parameters" do
      assert {:error, "Invalid parameters"} = Alerting.send_alert(nil, %{})
      assert {:error, "Invalid parameters"} = Alerting.send_alert("test", nil)
    end

    test "handles empty details map" do
      assert {:ok, "alert-sent"} = Alerting.send_alert("security_breach", %{})
    end

    test "handles complex details structure" do
      details = %{
        "user" => %{
          "id" => "789",
          "email" => "test@example.com",
          "last_login" => "2025-01-01T00:00:00Z"
        },
        "session" => %{
          "id" => "session_123",
          "ip" => "10.0.0.1",
          "user_agent" => "Mozilla/5.0"
        },
        "threat_level" => "high",
        "indicators" => ["failed_login", "unusual_location", "suspicious_timing"]
      }

      assert {:ok, "alert-sent"} = Alerting.send_alert("security_breach", details)
    end
  end

  describe "alert types" do
    test "handles all supported alert types" do
      details = %{"test" => "data"}

      assert {:ok, "alert-sent"} = Alerting.send_alert("security_breach", details)
      assert {:ok, "alert-sent"} = Alerting.send_alert("suspicious_activity", details)
      assert {:ok, "alert-sent"} = Alerting.send_alert("custom_alert", details)
    end
  end

  describe "edge cases" do
    test "handles very large details map" do
      details =
        for i <- 1..1000, into: %{} do
          {"key_#{i}", "value_#{i}"}
        end

      assert {:ok, "alert-sent"} = Alerting.send_alert("security_breach", details)
    end

    test "handles special characters in alert type" do
      details = %{"test" => "data"}

      assert {:ok, "alert-sent"} = Alerting.send_alert("alert-with-dashes", details)
      assert {:ok, "alert-sent"} = Alerting.send_alert("alert_with_underscores", details)
      assert {:ok, "alert-sent"} = Alerting.send_alert("ALERT_IN_UPPERCASE", details)
    end

    test "handles unicode characters in details" do
      details = %{
        "message" => "🚨 Security alert with emoji",
        "description" => "Café security breach detected",
        "user_name" => "José María"
      }

      assert {:ok, "alert-sent"} = Alerting.send_alert("security_breach", details)
    end
  end
end
