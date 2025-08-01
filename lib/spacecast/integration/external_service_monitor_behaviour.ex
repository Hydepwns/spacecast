defmodule Spacecast.Integration.ExternalServiceMonitorBehaviour do
  @moduledoc """
  Behaviour for external service monitor implementations.
  """

  @callback check_health(String.t()) :: {:ok, map()} | {:error, term()}
  @callback detect_service_failure(String.t()) ::
              {:ok, map()} | {:ok, :healthy} | {:error, term()}
  @callback track_metric(String.t(), String.t(), number()) :: {:ok, String.t()} | {:error, term()}
  @callback get_service_status_summary(list()) :: {:ok, map()} | {:error, term()}
  @callback setup_service_alert(String.t(), map()) :: {:ok, String.t()} | {:error, term()}
end
