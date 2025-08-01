defmodule Spacecast.Security.Detector do
  @moduledoc """
  Security threat detection functionality.

  This module provides functions for detecting suspicious activity
  and potential security threats in the application.
  """

  @doc """
  Detects suspicious activity based on the provided activity data.

  ## Parameters
  - `activity` - A map containing activity details

  ## Returns
  - `{:warning, message}` when suspicious activity is detected
  - `{:ok, "no-threat"}` when no suspicious activity is detected
  - `{:error, reason}` on error
  """
  def detect_suspicious_activity(activity) when is_map(activity) do
    case activity do
      %{"type" => "multiple_failed_logins", "count" => count} when count > 5 ->
        {:warning, "Suspicious activity detected"}

      %{"type" => "rapid_requests", "count" => count} when count > 100 ->
        {:warning, "Potential DDoS attack detected"}

      %{"type" => "unusual_access_pattern", "details" => _details} ->
        {:warning, "Unusual access pattern detected"}

      _ ->
        {:ok, "no-threat"}
    end
  end

  def detect_suspicious_activity(_activity) do
    {:error, "Invalid activity data"}
  end
end
