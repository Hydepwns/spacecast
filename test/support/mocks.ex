defmodule Spacecast.Mocks do
  @moduledoc """
  Defines mocks for external dependencies used in tests.

  This module defines mock behaviors that can be used to substitute
  real implementations during tests.
  """

  # Define behavior for HTTP client
  defmodule HTTPClientBehaviour do
    @moduledoc """
    Behavior for HTTP client implementations.
    """

    @callback get(String.t(), list(), keyword()) ::
                {:ok, %{status: integer(), body: String.t(), headers: list()}}
                | {:error, term()}

    @callback post(String.t(), map(), list(), keyword()) ::
                {:ok, %{status: integer(), body: String.t(), headers: list()}}
                | {:error, term()}
  end

  # Define behavior for external API client
  defmodule ExternalAPIBehaviour do
    @moduledoc """
    Behavior for external API client implementations.
    """

    @callback fetch_data(String.t()) :: {:ok, map()} | {:error, term()}
    @callback update_resource(String.t(), map()) :: {:ok, map()} | {:error, term()}
    @callback delete_resource(String.t()) :: :ok | {:error, term()}
  end

  # Define behavior for Security Logger
  defmodule SecurityLoggerBehaviour do
    @moduledoc """
    Behavior for Security Logger implementations.
    """

    @callback log_security_event(String.t(), map()) :: {:ok, String.t()} | {:error, term()}
  end

  # Define behavior for Security Detector
  defmodule SecurityDetectorBehaviour do
    @moduledoc """
    Behavior for Security Detector implementations.
    """

    @callback detect_suspicious_activity(map()) ::
                {:warning, String.t()} | {:ok, String.t()} | {:error, term()}
  end

  # Define behavior for Security Alerting
  defmodule SecurityAlertingBehaviour do
    @moduledoc """
    Behavior for Security Alerting implementations.
    """

    @callback send_alert(String.t(), map()) :: {:ok, String.t()} | {:error, term()}
  end

  # Define behavior for Performance Monitor
  defmodule PerformanceMonitorBehaviour do
    @moduledoc """
    Behavior for Performance Monitor implementations.
    """

    @callback track_response_time(String.t(), integer()) :: {:ok, String.t()} | {:error, term()}
    @callback check_performance() :: {:warning, map()} | {:ok, map()} | {:error, term()}
    @callback generate_report(String.t()) :: {:ok, map()} | {:error, term()}
  end

  # Define behavior for Performance Cache
  defmodule PerformanceCacheBehaviour do
    @moduledoc """
    Behavior for Performance Cache implementations.
    """

    @callback get(String.t()) :: {:ok, term()} | {:error, String.t()}
    @callback put(String.t(), term(), integer()) :: {:ok, String.t()} | {:error, term()}
    @callback delete(String.t()) :: {:ok, String.t()} | {:error, term()}
  end

  # Define implementation modules
  defmodule DefaultHTTPClient do
    @moduledoc """
    Default implementation of HTTP client behavior.

    This module provides a real implementation that would be used in production.
    """

    @behaviour HTTPClientBehaviour

    @impl true
    def get(_url, _headers \\ [], _opts \\ []) do
      # This would be a real HTTP request in production
      # We're providing a simple implementation for use with mocks
      {:ok, %{status: 200, body: "{}", headers: []}}
    end

    @impl true
    def post(_url, _body, _headers \\ [], _opts \\ []) do
      # This would be a real HTTP request in production
      {:ok, %{status: 201, body: "{}", headers: []}}
    end
  end

  defmodule DefaultExternalAPI do
    @moduledoc """
    Default implementation of external API behavior.

    This module provides a real implementation that would be used in production.
    """

    @behaviour ExternalAPIBehaviour

    @impl true
    def fetch_data(id) do
      # Mocked resource with all expected fields for integration tests
      {:ok,
       %{
         "id" => id,
         "name" => "Test Resource",
         "description" => "A resource for testing.",
         "type" => "test-type",
         "status" => "active",
         "content" => "This is the test content.",
         "html_content" => "<b>Test HTML Content</b>"
       }}
    end

    @impl true
    def update_resource(id, data) do
      # Real implementation would call external API
      {:ok, Map.put(data, "id", id)}
    end

    @impl true
    def delete_resource(_id) do
      # Real implementation would call external API
      :ok
    end
  end

  # Default implementation for Security Logger
  defmodule DefaultSecurityLogger do
    @moduledoc """
    Default implementation of Security Logger behavior.
    """

    @behaviour SecurityLoggerBehaviour

    @impl true
    def log_security_event(event_type, details) do
      Spacecast.Security.Logger.log_security_event(event_type, details)
    end
  end

  # Default implementation for Security Detector
  defmodule DefaultSecurityDetector do
    @moduledoc """
    Default implementation of Security Detector behavior.
    """

    @behaviour SecurityDetectorBehaviour

    @impl true
    def detect_suspicious_activity(activity) do
      Spacecast.Security.Detector.detect_suspicious_activity(activity)
    end
  end

  # Default implementation for Security Alerting
  defmodule DefaultSecurityAlerting do
    @moduledoc """
    Default implementation of Security Alerting behavior.
    """

    @behaviour SecurityAlertingBehaviour

    @impl true
    def send_alert(alert_type, details) do
      Spacecast.Security.Alerting.send_alert(alert_type, details)
    end
  end

  # Default implementation for Performance Monitor
  defmodule DefaultPerformanceMonitor do
    @moduledoc """
    Default implementation of Performance Monitor behavior.
    """

    @behaviour PerformanceMonitorBehaviour

    @impl true
    def track_response_time(endpoint, response_time) do
      Spacecast.Performance.Monitor.track_response_time(endpoint, response_time)
    end

    @impl true
    def check_performance do
      Spacecast.Performance.Monitor.check_performance()
    end

    @impl true
    def generate_report(time_range) do
      Spacecast.Performance.Monitor.generate_report(time_range)
    end
  end

  # Default implementation for Performance Cache
  defmodule DefaultPerformanceCache do
    @moduledoc """
    Default implementation of Performance Cache behavior.
    """

    @behaviour PerformanceCacheBehaviour

    @impl true
    def get(key) do
      Spacecast.Performance.Cache.get(key)
    end

    @impl true
    def put(key, data, ttl) do
      Spacecast.Performance.Cache.put(key, data, ttl)
    end

    @impl true
    def delete(key) do
      Spacecast.Performance.Cache.delete(key)
    end
  end
end

# Define mocks
Mox.defmock(Spacecast.MockHTTPClient, for: Spacecast.Mocks.HTTPClientBehaviour)
Mox.defmock(Spacecast.MockExternalAPI, for: Spacecast.Mocks.ExternalAPIBehaviour)

Mox.defmock(Spacecast.Notifications.EmailAdapter,
  for: Spacecast.Notifications.EmailAdapterBehaviour
)

Mox.defmock(Spacecast.Notifications.Twilio,
  for: Spacecast.Notifications.TwilioBehaviour
)

Mox.defmock(Spacecast.Integration.WebhookAdapter,
  for: Spacecast.Integration.WebhookAdapterBehaviour
)

Mox.defmock(Spacecast.Integration.ExternalSyncAdapter,
  for: Spacecast.Integration.ExternalSyncAdapterBehaviour
)

Mox.defmock(Spacecast.Integration.ExternalServiceMonitor,
  for: Spacecast.Integration.ExternalServiceMonitorBehaviour
)

Mox.defmock(Spacecast.Integration.ExternalServiceAuth,
  for: Spacecast.Integration.ExternalServiceAuthBehaviour
)

Mox.defmock(Spacecast.Integration.CircuitBreaker,
  for: Spacecast.Integration.CircuitBreakerBehaviour
)

Mox.defmock(Spacecast.Integration.AnalyticsAdapter,
  for: Spacecast.Integration.AnalyticsAdapterBehaviour
)

Mox.defmock(Spacecast.Events.Adapters.PushAdapter,
  for: Spacecast.Events.Adapters.PushAdapterBehaviour
)

Mox.defmock(Spacecast.MockSecurityLogger,
  for: Spacecast.Mocks.SecurityLoggerBehaviour
)

Mox.defmock(Spacecast.MockSecurityDetector,
  for: Spacecast.Mocks.SecurityDetectorBehaviour
)

Mox.defmock(Spacecast.MockSecurityAlerting,
  for: Spacecast.Mocks.SecurityAlertingBehaviour
)

Mox.defmock(Spacecast.MockPerformanceMonitor,
  for: Spacecast.Mocks.PerformanceMonitorBehaviour
)

Mox.defmock(Spacecast.MockPerformanceCache,
  for: Spacecast.Mocks.PerformanceCacheBehaviour
)

defmodule Spacecast.DefaultHTTPClient do
  @moduledoc false
  @behaviour Spacecast.Mocks.HTTPClientBehaviour

  defdelegate get(url, headers \\ [], opts \\ []), to: Spacecast.Mocks.DefaultHTTPClient

  defdelegate post(url, body, headers \\ [], opts \\ []),
    to: Spacecast.Mocks.DefaultHTTPClient
end

defmodule Spacecast.DefaultExternalAPI do
  @moduledoc false
  @behaviour Spacecast.Mocks.ExternalAPIBehaviour

  defdelegate fetch_data(id), to: Spacecast.Mocks.DefaultExternalAPI
  defdelegate update_resource(id, data), to: Spacecast.Mocks.DefaultExternalAPI
  defdelegate delete_resource(id), to: Spacecast.Mocks.DefaultExternalAPI
end

defmodule Spacecast.DefaultSecurityLogger do
  @moduledoc false
  @behaviour Spacecast.Mocks.SecurityLoggerBehaviour

  defdelegate log_security_event(event_type, details),
    to: Spacecast.Mocks.DefaultSecurityLogger
end

defmodule Spacecast.DefaultSecurityDetector do
  @moduledoc false
  @behaviour Spacecast.Mocks.SecurityDetectorBehaviour

  defdelegate detect_suspicious_activity(activity),
    to: Spacecast.Mocks.DefaultSecurityDetector
end

defmodule Spacecast.DefaultSecurityAlerting do
  @moduledoc false
  @behaviour Spacecast.Mocks.SecurityAlertingBehaviour

  defdelegate send_alert(alert_type, details), to: Spacecast.Mocks.DefaultSecurityAlerting
end

defmodule Spacecast.DefaultPerformanceMonitor do
  @moduledoc false
  @behaviour Spacecast.Mocks.PerformanceMonitorBehaviour

  defdelegate track_response_time(endpoint, response_time),
    to: Spacecast.Mocks.DefaultPerformanceMonitor

  defdelegate check_performance(), to: Spacecast.Mocks.DefaultPerformanceMonitor
  defdelegate generate_report(time_range), to: Spacecast.Mocks.DefaultPerformanceMonitor
end

defmodule Spacecast.DefaultPerformanceCache do
  @moduledoc false
  @behaviour Spacecast.Mocks.PerformanceCacheBehaviour

  defdelegate get(key), to: Spacecast.Mocks.DefaultPerformanceCache
  defdelegate put(key, data, ttl), to: Spacecast.Mocks.DefaultPerformanceCache
  defdelegate delete(key), to: Spacecast.Mocks.DefaultPerformanceCache
end
