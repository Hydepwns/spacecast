defmodule Spacecast.Integration.CircuitBreakerBehaviour do
  @moduledoc """
  Behaviour for circuit breaker implementations.
  """

  @callback call(String.t(), String.t(), function()) :: {:ok, term()} | {:error, term()}
  @callback get_state(String.t(), String.t()) :: :closed | :open | :half_open
  @callback reset(String.t(), String.t()) :: :ok
  @callback get_stats(String.t(), String.t()) :: {:ok, map()} | {:error, term()}
end
