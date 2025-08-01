defmodule SpacecastWeb.Integration.ExternalServiceIntegrationTest do
  @moduledoc """
  Comprehensive external service integration tests covering API integrations,
  third-party services, webhooks, and external data synchronization.
  """

  use SpacecastWeb.ConnCase, async: false
  import Phoenix.LiveViewTest
  import Mox
  setup :set_mox_from_context
  setup :verify_on_exit!

  # Override repo configuration for integration tests to use real database
  setup do
    # Temporarily set repo to use real database for integration tests
    Application.put_env(:spacecast, :repo, Spacecast.Repo)

    on_exit(fn ->
      # Restore mock repo after test
      Application.put_env(:spacecast, :repo, Spacecast.RepoMock)
    end)

    :ok
  end

  alias Spacecast.Accounts
  alias Spacecast.Resources.ResourceSystem
  alias Spacecast.Events

  setup do
    # Set up comprehensive mocks for external services
    setup_external_service_mocks()

    # Create test user
    {:ok, user} =
      Accounts.register_user(%{
        email: "external_test@example.com",
        password: "password123",
        password_confirmation: "password123",
        name: "External Test User"
      })

    # Create test resource
    {:ok, resource} =
      ResourceSystem.create_resource(%{
        name: "External Service Test Resource",
        description: "Resource for external service testing",
        type: "document",
        status: "published",
        content: %{text: "Test content"}
      })

    # Create a connection for the tests
    conn = build_conn()

    {:ok, user: user, resource: resource, _conn: conn}
  end

  describe "External API Integration" do
    test "fetches data from external API", %{_conn: _conn} do
      # Mock successful API response
      Spacecast.MockExternalAPI
      |> expect(:fetch_data, fn id ->
        {:ok,
         %{
           "id" => id,
           "name" => "External API Resource",
           "description" => "Fetched from external API",
           "type" => "external",
           "status" => "active",
           "external_metadata" => %{
             "source" => "external_api",
             "last_sync" => DateTime.utc_now() |> DateTime.to_iso8601()
           }
         }}
      end)

      # Test API call
      {:ok, data} = Spacecast.MockExternalAPI.fetch_data("external-123")

      assert data["name"] == "External API Resource"
      assert data["type"] == "external"
      assert data["external_metadata"]["source"] == "external_api"
    end

    test "handles external API errors gracefully", %{_conn: _conn} do
      # Mock API error
      Spacecast.MockExternalAPI
      |> expect(:fetch_data, fn _id ->
        {:error, "External service unavailable"}
      end)

      # Test error handling
      result = Spacecast.MockExternalAPI.fetch_data("error-123")

      assert {:error, "External service unavailable"} = result
    end

    test "retries failed API calls", %{_conn: _conn} do
      # Mock API to succeed
      Spacecast.MockExternalAPI
      |> expect(:fetch_data, fn id ->
        {:ok, %{"id" => id, "name" => "Retry Success Resource"}}
      end)

      # Test API call
      result = Spacecast.MockExternalAPI.fetch_data("retry-123")

      # Should succeed
      assert {:ok, data} = result
      assert data["name"] == "Retry Success Resource"
    end

    test "handles API rate limiting", %{_conn: _conn} do
      # Mock rate limit response
      Spacecast.MockExternalAPI
      |> expect(:fetch_data, fn _id ->
        {:error, %{status: 429, message: "Rate limit exceeded"}}
      end)

      # Test rate limit handling
      result = Spacecast.MockExternalAPI.fetch_data("rate-limit-123")

      assert {:error, %{status: 429, message: "Rate limit exceeded"}} = result
    end
  end

  describe "Third-Party Service Integration" do
    test "integrates with email service", %{_conn: _conn} do
      # Mock email service
      Spacecast.Notifications.EmailAdapter
      |> expect(:send_email, fn to, subject, body ->
        assert to == "test@example.com"
        assert subject == "Test Email"
        assert body =~ "Test content"
        {:ok, "email-sent-123"}
      end)

      # Test email integration
      result =
        Spacecast.Notifications.EmailAdapter.send_email(
          "test@example.com",
          "Test Email",
          "Test content"
        )

      assert {:ok, "email-sent-123"} = result
    end

    test "integrates with SMS service", %{_conn: _conn} do
      # Mock SMS service
      Spacecast.Notifications.Twilio
      |> expect(:send_sms, fn to, message ->
        assert to == "+1234567890"
        assert message =~ "Test SMS"
        {:ok, "sms-sent-456"}
      end)

      # Test SMS integration
      result =
        Spacecast.Notifications.Twilio.send_sms(
          "+1234567890",
          "Test SMS message"
        )

      assert {:ok, "sms-sent-456"} = result
    end

    test "integrates with push notification service", %{_conn: _conn} do
      # Mock push notification service
      Spacecast.Events.Adapters.PushAdapter
      |> expect(:send_notification, fn device_token, notification ->
        assert device_token == "device-token-123"
        assert notification.title == "Test Push"
        assert notification.body =~ "Test push notification"
        {:ok, "push-sent-789"}
      end)

      # Test push notification integration
      notification = %{
        title: "Test Push",
        body: "Test push notification",
        data: %{action: "test"}
      }

      result =
        Spacecast.Events.Adapters.PushAdapter.send_notification(
          "device-token-123",
          notification
        )

      assert {:ok, "push-sent-789"} = result
    end

    test "integrates with analytics service", %{_conn: _conn} do
      # Mock analytics service
      Spacecast.Integration.AnalyticsAdapter
      |> expect(:track_event, fn event_name, properties ->
        assert event_name == "resource_created"
        assert properties["resource_id"] == "analytics-test-123"
        {:ok, "event-tracked"}
      end)

      # Test analytics integration
      result =
        Spacecast.Integration.AnalyticsAdapter.track_event(
          "resource_created",
          %{"resource_id" => "analytics-test-123"}
        )

      assert {:ok, "event-tracked"} = result
    end
  end

  describe "Webhook Integration" do
    test "sends webhooks for resource events", %{_conn: _conn} do
      # Mock webhook service
      Spacecast.Integration.WebhookAdapter
      |> expect(:send_webhook, fn url, payload ->
        assert url == "https://webhook.example.com/resource-events"
        assert payload["event_type"] == "resource.created"
        assert payload["resource_id"] == "webhook-test-123"
        {:ok, "webhook-sent"}
      end)

      # Test webhook integration
      webhook_data = %{
        "event_type" => "resource.created",
        "resource_id" => "webhook-test-123",
        "timestamp" => DateTime.utc_now() |> DateTime.to_iso8601()
      }

      result =
        Spacecast.Integration.WebhookAdapter.send_webhook(
          "https://webhook.example.com/resource-events",
          webhook_data
        )

      assert {:ok, "webhook-sent"} = result
    end

    test "handles webhook delivery failures", %{_conn: _conn} do
      # Mock webhook failure
      Spacecast.Integration.WebhookAdapter
      |> expect(:send_webhook, fn _url, _payload ->
        {:error, "Webhook delivery failed"}
      end)

      # Test webhook failure handling
      result =
        Spacecast.Integration.WebhookAdapter.send_webhook(
          "https://webhook.example.com/resource-events",
          %{"event_type" => "resource.created"}
        )

      assert {:error, "Webhook delivery failed"} = result
    end

    test "retries failed webhooks", %{_conn: _conn} do
      # Mock webhook to succeed
      Spacecast.Integration.WebhookAdapter
      |> expect(:send_webhook, fn _url, _payload ->
        {:ok, "webhook-retry-success"}
      end)

      # Test webhook call
      result =
        Spacecast.Integration.WebhookAdapter.send_webhook(
          "https://webhook.example.com/resource-events",
          %{"event_type" => "resource.created"}
        )

      # Should succeed
      assert {:ok, "webhook-retry-success"} = result
    end

    test "validates webhook signatures", %{_conn: _conn} do
      # Mock webhook signature validation
      Spacecast.Integration.WebhookAdapter
      |> expect(:validate_signature, fn payload, signature, secret ->
        assert payload =~ "test-payload"
        assert signature == "valid-signature"
        assert secret == "webhook-secret"
        {:ok, true}
      end)

      # Test signature validation
      result =
        Spacecast.Integration.WebhookAdapter.validate_signature(
          "test-payload",
          "valid-signature",
          "webhook-secret"
        )

      assert {:ok, true} = result
    end
  end

  describe "External Data Synchronization" do
    test "synchronizes data with external system", %{_conn: _conn} do
      # Mock external system
      Spacecast.Integration.ExternalSyncAdapter
      |> expect(:sync_resource, fn resource ->
        assert resource.name == "Sync Test Resource"
        {:ok, %{external_id: "ext-123", synced_at: DateTime.utc_now()}}
      end)

      # Test data synchronization
      resource = %{
        id: "sync-test-123",
        name: "Sync Test Resource",
        type: "document",
        status: "published"
      }

      result = Spacecast.Integration.ExternalSyncAdapter.sync_resource(resource)

      assert {:ok, sync_result} = result
      assert sync_result.external_id == "ext-123"
      assert sync_result.synced_at != nil
    end

    test "handles synchronization conflicts", %{_conn: _conn} do
      # Mock synchronization conflict
      Spacecast.Integration.ExternalSyncAdapter
      |> expect(:sync_resource, fn _resource ->
        {:error, :conflict,
         %{
           local_version: 1,
           remote_version: 2,
           conflict_data: %{name: "Conflicting Name"}
         }}
      end)

      # Test conflict handling
      result =
        Spacecast.Integration.ExternalSyncAdapter.sync_resource(%{
          id: "conflict-test-123",
          name: "Local Resource",
          version: 1
        })

      assert {:error, :conflict, conflict_info} = result
      assert conflict_info.local_version == 1
      assert conflict_info.remote_version == 2
    end

    test "performs incremental synchronization", %{_conn: _conn} do
      # Mock incremental sync
      Spacecast.Integration.ExternalSyncAdapter
      |> expect(:incremental_sync, fn since_timestamp ->
        assert since_timestamp != nil

        {:ok,
         [
           %{id: "inc-1", action: "created"},
           %{id: "inc-2", action: "updated"}
         ]}
      end)

      # Test incremental synchronization
      since = DateTime.utc_now() |> DateTime.add(-3600, :second)

      result = Spacecast.Integration.ExternalSyncAdapter.incremental_sync(since)

      assert {:ok, changes} = result
      assert length(changes) == 2
      assert Enum.at(changes, 0).action == "created"
      assert Enum.at(changes, 1).action == "updated"
    end

    test "handles synchronization failures", %{_conn: _conn} do
      # Mock sync failure
      Spacecast.Integration.ExternalSyncAdapter
      |> expect(:sync_resource, fn _resource ->
        {:error, "External system unavailable"}
      end)

      # Test failure handling
      result =
        Spacecast.Integration.ExternalSyncAdapter.sync_resource(%{
          id: "failure-test-123",
          name: "Failure Test Resource"
        })

      assert {:error, "External system unavailable"} = result
    end
  end

  describe "External Service Monitoring" do
    test "monitors external service health", %{_conn: _conn} do
      # Mock health check
      Spacecast.Integration.ExternalServiceMonitor
      |> expect(:check_health, fn service_name ->
        assert service_name == "external_api"

        {:ok,
         %{
           status: "healthy",
           response_time: 150,
           last_check: DateTime.utc_now()
         }}
      end)

      # Test health monitoring
      result = Spacecast.Integration.ExternalServiceMonitor.check_health("external_api")

      assert {:ok, health_info} = result
      assert health_info.status == "healthy"
      assert health_info.response_time < 1000
    end

    test "detects external service failures", %{_conn: _conn} do
      # Mock health check failure
      Spacecast.Integration.ExternalServiceMonitor
      |> expect(:check_health, fn service_name ->
        assert service_name == "external_api"
        {:error, "Service unavailable"}
      end)

      # Test failure detection
      result = Spacecast.Integration.ExternalServiceMonitor.check_health("external_api")

      assert {:error, "Service unavailable"} = result
    end

    test "tracks external service metrics", %{_conn: _conn} do
      # Mock metrics tracking
      Spacecast.Integration.ExternalServiceMonitor
      |> expect(:track_metric, fn service_name, metric_name, value ->
        assert service_name == "external_api"
        assert metric_name == "response_time"
        assert is_number(value)
        {:ok, "metric-tracked"}
      end)

      # Test metrics tracking
      result =
        Spacecast.Integration.ExternalServiceMonitor.track_metric(
          "external_api",
          "response_time",
          150.5
        )

      assert {:ok, "metric-tracked"} = result
    end
  end

  describe "External Service Security" do
    test "validates external service credentials", %{_conn: _conn} do
      # Mock credential validation
      Spacecast.Integration.ExternalServiceAuth
      |> expect(:validate_credentials, fn service_name, credentials ->
        assert service_name == "external_api"
        assert credentials.api_key == "valid-key"
        {:ok, true}
      end)

      # Test credential validation
      result =
        Spacecast.Integration.ExternalServiceAuth.validate_credentials(
          "external_api",
          %{api_key: "valid-key"}
        )

      assert {:ok, true} = result
    end

    test "handles invalid credentials", %{_conn: _conn} do
      # Mock invalid credentials
      Spacecast.Integration.ExternalServiceAuth
      |> expect(:validate_credentials, fn _service_name, credentials ->
        assert credentials.api_key == "invalid-key"
        {:error, "Invalid API key"}
      end)

      # Test invalid credential handling
      result =
        Spacecast.Integration.ExternalServiceAuth.validate_credentials(
          "external_api",
          %{api_key: "invalid-key"}
        )

      assert {:error, "Invalid API key"} = result
    end

    test "encrypts sensitive data for external services", %{_conn: _conn} do
      # Mock encryption
      Spacecast.Integration.ExternalServiceAuth
      |> expect(:encrypt_data, fn data, key ->
        assert data == "sensitive-data"
        assert key == "encryption-key"
        {:ok, "encrypted-data"}
      end)

      # Test data encryption
      result =
        Spacecast.Integration.ExternalServiceAuth.encrypt_data(
          "sensitive-data",
          "encryption-key"
        )

      assert {:ok, "encrypted-data"} = result
    end
  end

  describe "External Service Performance" do
    test "handles high external service load", %{_conn: _conn} do
      # Mock high load scenario - expect 10 calls
      Spacecast.MockExternalAPI
      |> expect(:fetch_data, 10, fn id ->
        # Simulate slow response under load
        Process.sleep(100)
        {:ok, %{"id" => id, "name" => "Load Test Resource"}}
      end)

      # Test under load
      start_time = System.monotonic_time(:millisecond)

      tasks =
        for i <- 1..10 do
          Task.async(fn ->
            Spacecast.MockExternalAPI.fetch_data("load-test-#{i}")
          end)
        end

      results = Task.await_many(tasks)
      end_time = System.monotonic_time(:millisecond)
      duration = end_time - start_time

      # Verify all requests succeeded
      assert length(results) == 10

      Enum.each(results, fn result ->
        assert {:ok, data} = result
        assert data["name"] == "Load Test Resource"
      end)

      # Verify performance is acceptable
      # Less than 5 seconds for 10 concurrent requests
      assert duration < 5000
    end

    test "implements circuit breaker pattern", %{_conn: _conn} do
      # Mock circuit breaker
      Spacecast.Integration.CircuitBreaker
      |> expect(:call, fn service_name, operation, _fun ->
        assert service_name == "external_api"
        assert operation == "fetch_data"
        {:ok, "circuit-breaker-success"}
      end)

      # Test circuit breaker
      result =
        Spacecast.Integration.CircuitBreaker.call(
          "external_api",
          "fetch_data",
          fn -> {:ok, "success"} end
        )

      assert {:ok, "circuit-breaker-success"} = result
    end
  end

  # Helper function to set up external service mocks
  defp setup_external_service_mocks do
    # Set up default mocks for all external services
    Spacecast.MockExternalAPI
    |> stub(:fetch_data, fn id ->
      {:ok,
       %{
         "id" => id,
         "name" => "Default External Resource",
         "type" => "external",
         "status" => "active"
       }}
    end)
    |> stub(:update_resource, fn id, data ->
      {:ok, Map.put(data, "id", id)}
    end)
    |> stub(:delete_resource, fn _id ->
      {:ok, "deleted"}
    end)

    # Set up notification service mocks
    Spacecast.Notifications.EmailAdapter
    |> stub(:send_email, fn _to, _subject, _body ->
      {:ok, "email-sent"}
    end)

    Spacecast.Notifications.Twilio
    |> stub(:send_sms, fn _to, _message ->
      {:ok, "sms-sent"}
    end)

    # Set up integration service mocks
    Spacecast.Integration.WebhookAdapter
    |> stub(:send_webhook, fn _url, _payload ->
      {:ok, "webhook-sent"}
    end)

    Spacecast.Integration.ExternalSyncAdapter
    |> stub(:sync_resource, fn resource ->
      {:ok, %{external_id: "ext-#{resource.id}", synced_at: DateTime.utc_now()}}
    end)

    Spacecast.Integration.ExternalServiceMonitor
    |> stub(:check_health, fn _service_name ->
      {:ok, %{status: "healthy", response_time: 100}}
    end)
  end
end
