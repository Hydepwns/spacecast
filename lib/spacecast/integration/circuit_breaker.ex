defmodule Spacecast.Integration.CircuitBreaker do
  @moduledoc """
  Circuit breaker implementation for external service calls.

  This module provides a circuit breaker pattern to prevent
  cascading failures when external services are unavailable.
  """

  @behaviour Spacecast.Integration.CircuitBreakerBehaviour

  require Logger

  @type circuit_state :: :closed | :open | :half_open
  @type circuit_config :: %{
          failure_threshold: non_neg_integer(),
          timeout_ms: non_neg_integer(),
          reset_timeout_ms: non_neg_integer()
        }

  @doc """
  Executes a function with circuit breaker protection.

  ## Parameters
  - service_name: Name of the service being called
  - operation: Name of the operation being performed
  - fun: Function to execute

  ## Returns
  - {:ok, result} on success
  - {:error, reason} on failure
  """
  def call(service_name, operation, fun) when is_function(fun, 0) do
    circuit_key = "#{service_name}:#{operation}"
    config = get_circuit_config(service_name)

    case get_circuit_state(circuit_key) do
      :closed ->
        execute_with_circuit_breaker(circuit_key, fun, config)

      :open ->
        case should_attempt_reset?(circuit_key, config) do
          true ->
            set_circuit_state(circuit_key, :half_open)
            execute_with_circuit_breaker(circuit_key, fun, config)

          false ->
            Logger.warning("Circuit breaker open for #{circuit_key}")
            {:error, :circuit_open}
        end

      :half_open ->
        execute_with_circuit_breaker(circuit_key, fun, config)
    end
  end

  @doc """
  Gets the current state of a circuit breaker.

  ## Parameters
  - service_name: Name of the service
  - operation: Name of the operation

  ## Returns
  - circuit_state: Current state of the circuit breaker
  """
  def get_state(service_name, operation) do
    circuit_key = "#{service_name}:#{operation}"
    get_circuit_state(circuit_key)
  end

  @doc """
  Manually resets a circuit breaker to closed state.

  ## Parameters
  - service_name: Name of the service
  - operation: Name of the operation

  ## Returns
  - :ok on success
  """
  def reset(service_name, operation) do
    circuit_key = "#{service_name}:#{operation}"
    set_circuit_state(circuit_key, :closed)
    reset_failure_count(circuit_key)
    Logger.info("Circuit breaker manually reset for #{circuit_key}")
    :ok
  end

  @doc """
  Gets statistics for a circuit breaker.

  ## Parameters
  - service_name: Name of the service
  - operation: Name of the operation

  ## Returns
  - {:ok, stats} on success
  - {:error, reason} on failure
  """
  def get_stats(service_name, operation) do
    circuit_key = "#{service_name}:#{operation}"

    stats = %{
      state: get_circuit_state(circuit_key),
      failure_count: get_failure_count(circuit_key),
      last_failure_time: get_last_failure_time(circuit_key),
      last_success_time: get_last_success_time(circuit_key),
      total_calls: get_total_calls(circuit_key)
    }

    {:ok, stats}
  end

  # Private functions

  defp execute_with_circuit_breaker(circuit_key, fun, config) do
    increment_total_calls(circuit_key)

    case execute_with_timeout(fun, config.timeout_ms) do
      {:ok, result} ->
        handle_success(circuit_key)
        {:ok, result}

      {:error, reason} ->
        handle_failure(circuit_key, config)
        {:error, reason}
    end
  end

  defp execute_with_timeout(fun, timeout_ms) do
    try do
      task = Task.async(fun)
      result = Task.await(task, timeout_ms)
      {:ok, result}
    catch
      :exit, {:timeout, _} ->
        {:error, :timeout}

      :exit, reason ->
        {:error, reason}

      kind, reason ->
        {:error, {kind, reason}}
    end
  end

  defp handle_success(circuit_key) do
    case get_circuit_state(circuit_key) do
      :half_open ->
        set_circuit_state(circuit_key, :closed)
        reset_failure_count(circuit_key)
        Logger.info("Circuit breaker reset to closed for #{circuit_key}")

      _ ->
        :ok
    end

    set_last_success_time(circuit_key, DateTime.utc_now())
  end

  defp handle_failure(circuit_key, config) do
    increment_failure_count(circuit_key)
    set_last_failure_time(circuit_key, DateTime.utc_now())

    failure_count = get_failure_count(circuit_key)

    if failure_count >= config.failure_threshold do
      set_circuit_state(circuit_key, :open)
      Logger.warning("Circuit breaker opened for #{circuit_key} after #{failure_count} failures")
    end
  end

  defp should_attempt_reset?(circuit_key, config) do
    case get_last_failure_time(circuit_key) do
      nil ->
        true

      last_failure ->
        time_since_failure = DateTime.diff(DateTime.utc_now(), last_failure, :millisecond)
        time_since_failure >= config.reset_timeout_ms
    end
  end

  defp get_circuit_config(_service_name) do
    # Default configuration - in production this would come from config
    %{
      failure_threshold: 5,
      timeout_ms: 5000,
      reset_timeout_ms: 30000
    }
  end

  # Circuit state management using ETS
  defp get_circuit_state(circuit_key) do
    case :ets.lookup(:circuit_breaker_state, circuit_key) do
      [{^circuit_key, state}] -> state
      [] -> :closed
    end
  end

  defp set_circuit_state(circuit_key, state) do
    :ets.insert(:circuit_breaker_state, {circuit_key, state})
  end

  defp get_failure_count(circuit_key) do
    case :ets.lookup(:circuit_breaker_failures, circuit_key) do
      [{^circuit_key, count}] -> count
      [] -> 0
    end
  end

  defp increment_failure_count(circuit_key) do
    count = get_failure_count(circuit_key)
    :ets.insert(:circuit_breaker_failures, {circuit_key, count + 1})
  end

  defp reset_failure_count(circuit_key) do
    :ets.insert(:circuit_breaker_failures, {circuit_key, 0})
  end

  defp get_last_failure_time(circuit_key) do
    case :ets.lookup(:circuit_breaker_failure_times, circuit_key) do
      [{^circuit_key, time}] -> time
      [] -> nil
    end
  end

  defp set_last_failure_time(circuit_key, time) do
    :ets.insert(:circuit_breaker_failure_times, {circuit_key, time})
  end

  defp get_last_success_time(circuit_key) do
    case :ets.lookup(:circuit_breaker_success_times, circuit_key) do
      [{^circuit_key, time}] -> time
      [] -> nil
    end
  end

  defp set_last_success_time(circuit_key, time) do
    :ets.insert(:circuit_breaker_success_times, {circuit_key, time})
  end

  defp get_total_calls(circuit_key) do
    case :ets.lookup(:circuit_breaker_total_calls, circuit_key) do
      [{^circuit_key, count}] -> count
      [] -> 0
    end
  end

  defp increment_total_calls(circuit_key) do
    count = get_total_calls(circuit_key)
    :ets.insert(:circuit_breaker_total_calls, {circuit_key, count + 1})
  end
end
