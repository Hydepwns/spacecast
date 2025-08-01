defmodule Spacecast.Integration.AnalyticsAdapterBehaviour do
  @moduledoc """
  Behaviour for analytics adapter implementations.
  """

  @callback track_event(String.t(), map()) :: {:ok, String.t()} | {:error, term()}
  @callback track_page_view(String.t(), map()) :: {:ok, String.t()} | {:error, term()}
  @callback track_user_action(String.t(), String.t(), map()) ::
              {:ok, String.t()} | {:error, term()}
  @callback track_conversion(String.t(), number(), map()) :: {:ok, String.t()} | {:error, term()}
  @callback track_metric(String.t(), number(), map()) :: {:ok, String.t()} | {:error, term()}
  @callback set_user_properties(String.t(), map()) :: {:ok, String.t()} | {:error, term()}
  @callback get_analytics_data(DateTime.t(), DateTime.t(), map()) ::
              {:ok, map()} | {:error, term()}
end
