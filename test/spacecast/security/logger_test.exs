defmodule Spacecast.Security.LoggerTest do
  use ExUnit.Case, async: true
  alias Spacecast.Security.Logger

  describe "log_security_event/2" do
    test "logs failed login event successfully" do
      details = %{
        "user_id" => "123",
        "ip_address" => "192.168.1.1",
        "timestamp" => "2025-01-01T00:00:00Z",
        "attempt_count" => 3
      }

      assert {:ok, "event-logged"} = Logger.log_security_event("failed_login", details)
    end

    test "logs security breach event successfully" do
      details = %{
        "user_id" => "456",
        "breach_type" => "unauthorized_access",
        "severity" => "high",
        "affected_resources" => ["user_data", "admin_panel"]
      }

      assert {:ok, "event-logged"} = Logger.log_security_event("security_breach", details)
    end

    test "logs suspicious activity event successfully" do
      details = %{
        "user_id" => "789",
        "activity_type" => "unusual_login_pattern",
        "risk_score" => 0.8,
        "indicators" => ["unusual_location", "unusual_time"]
      }

      assert {:ok, "event-logged"} = Logger.log_security_event("suspicious_activity", details)
    end

    test "logs custom security event successfully" do
      details = %{
        "message" => "Custom security event",
        "source" => "firewall",
        "action_taken" => "blocked_ip"
      }

      assert {:ok, "event-logged"} = Logger.log_security_event("custom_event", details)
    end

    test "returns error for invalid event type" do
      details = %{"message" => "Test event"}

      assert {:error, "Invalid parameters"} = Logger.log_security_event(123, details)
    end

    test "returns error for invalid details" do
      assert {:error, "Invalid parameters"} =
               Logger.log_security_event("failed_login", "invalid_details")
    end

    test "returns error for nil parameters" do
      assert {:error, "Invalid parameters"} = Logger.log_security_event(nil, %{})
      assert {:error, "Invalid parameters"} = Logger.log_security_event("test", nil)
    end

    test "handles empty details map" do
      assert {:ok, "event-logged"} = Logger.log_security_event("failed_login", %{})
    end
  end

  describe "event types" do
    test "handles various security event types" do
      details = %{"test" => "data"}

      assert {:ok, "event-logged"} = Logger.log_security_event("failed_login", details)
      assert {:ok, "event-logged"} = Logger.log_security_event("security_breach", details)
      assert {:ok, "event-logged"} = Logger.log_security_event("suspicious_activity", details)
      assert {:ok, "event-logged"} = Logger.log_security_event("password_reset", details)
      assert {:ok, "event-logged"} = Logger.log_security_event("account_locked", details)
      assert {:ok, "event-logged"} = Logger.log_security_event("ip_blocked", details)
    end

    test "handles event types with special characters" do
      details = %{"test" => "data"}

      assert {:ok, "event-logged"} = Logger.log_security_event("event-with-dashes", details)
      assert {:ok, "event-logged"} = Logger.log_security_event("event_with_underscores", details)
      assert {:ok, "event-logged"} = Logger.log_security_event("EVENT_IN_UPPERCASE", details)
    end
  end

  describe "complex event details" do
    test "handles complex user session details" do
      details = %{
        "user" => %{
          "id" => "123",
          "email" => "user@example.com",
          "role" => "admin",
          "last_login" => "2025-01-01T00:00:00Z"
        },
        "session" => %{
          "id" => "session_456",
          "ip_address" => "10.0.0.1",
          "user_agent" => "Mozilla/5.0 (Windows NT 10.0; Win64; x64)",
          "location" => %{
            "country" => "US",
            "city" => "New York",
            "timezone" => "America/New_York"
          }
        },
        "event_context" => %{
          "endpoint" => "/api/admin/users",
          "method" => "POST",
          "request_id" => "req_789"
        }
      }

      assert {:ok, "event-logged"} = Logger.log_security_event("unauthorized_access", details)
    end

    test "handles security incident details" do
      details = %{
        "incident_id" => "INC-2025-001",
        "severity" => "critical",
        "category" => "data_breach",
        "affected_users" => 1500,
        "affected_data" => [
          "personal_information",
          "financial_data",
          "access_credentials"
        ],
        "timeline" => [
          %{"timestamp" => "2025-01-01T00:00:00Z", "event" => "breach_detected"},
          %{"timestamp" => "2025-01-01T00:05:00Z", "event" => "containment_started"},
          %{"timestamp" => "2025-01-01T00:10:00Z", "event" => "breach_contained"}
        ],
        "response_actions" => [
          "blocked_suspicious_ips",
          "reset_affected_passwords",
          "notified_users"
        ]
      }

      assert {:ok, "event-logged"} = Logger.log_security_event("security_breach", details)
    end

    test "handles threat intelligence details" do
      details = %{
        "threat_id" => "THREAT-2025-001",
        "threat_type" => "malware",
        "malware_family" => "ransomware",
        "indicators" => [
          %{"type" => "ip", "value" => "192.168.1.100", "confidence" => 0.9},
          %{"type" => "domain", "value" => "malicious.example.com", "confidence" => 0.8},
          %{"type" => "hash", "value" => "abc123def456", "confidence" => 0.95}
        ],
        "affected_systems" => ["web_server", "database_server"],
        "mitigation_status" => "in_progress"
      }

      assert {:ok, "event-logged"} = Logger.log_security_event("threat_detected", details)
    end
  end

  describe "edge cases" do
    test "handles very large details map" do
      details =
        for i <- 1..1000, into: %{} do
          {"key_#{i}", "value_#{i}"}
        end

      assert {:ok, "event-logged"} = Logger.log_security_event("test_event", details)
    end

    test "handles unicode characters in event type and details" do
      details = %{
        "message" => "🚨 Security incident with emoji",
        "description" => "Café security breach detected",
        "user_name" => "José María",
        "location" => "São Paulo"
      }

      assert {:ok, "event-logged"} = Logger.log_security_event("incident_report", details)
    end

    test "handles deeply nested structures" do
      details = %{
        "level1" => %{
          "level2" => %{
            "level3" => %{
              "level4" => %{
                "level5" => %{
                  "value" => "deeply_nested"
                }
              }
            }
          }
        }
      }

      assert {:ok, "event-logged"} = Logger.log_security_event("nested_event", details)
    end

    test "handles lists in details" do
      details = %{
        "affected_ips" => ["192.168.1.1", "192.168.1.2", "192.168.1.3"],
        "actions_taken" => ["blocked", "logged", "alerted"],
        "metadata" => [
          %{"key" => "value1"},
          %{"key" => "value2"},
          %{"key" => "value3"}
        ]
      }

      assert {:ok, "event-logged"} = Logger.log_security_event("list_event", details)
    end
  end

  describe "data validation" do
    test "handles boolean values in details" do
      details = %{
        "is_suspicious" => true,
        "requires_immediate_action" => false,
        "has_been_escalated" => true
      }

      assert {:ok, "event-logged"} = Logger.log_security_event("boolean_event", details)
    end

    test "handles numeric values in details" do
      details = %{
        "attempt_count" => 5,
        "risk_score" => 0.75,
        "response_time_ms" => 150
      }

      assert {:ok, "event-logged"} = Logger.log_security_event("numeric_event", details)
    end

    test "handles atom values in details" do
      details = %{
        "status" => :active,
        "priority" => :high,
        "type" => :security
      }

      assert {:ok, "event-logged"} = Logger.log_security_event("atom_event", details)
    end
  end
end
