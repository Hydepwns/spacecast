defmodule SpacecastWeb.PerformanceMonitor do
  @moduledoc """
  Performance monitoring utilities for Wallaby tests.

  This module provides tools for:
  - Measuring test execution times
  - Identifying slow operations
  - Tracking performance regressions
  - Generating performance reports
  """

  import Wallaby.Browser, only: [visit: 2]

  @doc """
  Measures the execution time of a function and logs the result.

  ## Parameters
  - operation_name: Name of the operation being measured
  - fun: Function to measure
  - threshold_ms: Warning threshold in milliseconds (default: 1000)

  ## Returns
  {result, execution_time_ms}
  """
  def measure_operation(operation_name, fun, threshold_ms \\ 1000) do
    start_time = System.monotonic_time(:millisecond)
    result = fun.()
    end_time = System.monotonic_time(:millisecond)
    execution_time = end_time - start_time

    log_performance(operation_name, execution_time, threshold_ms)
    {result, execution_time}
  end

  @doc """
  Measures Wallaby session operations and logs performance.

  ## Parameters
  - session: Wallaby session
  - operation_name: Name of the operation
  - operation_fun: Function that takes session and returns {session, result}
  - threshold_ms: Warning threshold in milliseconds

  ## Returns
  {session, result, execution_time_ms}
  """
  def measure_session_operation(session, operation_name, operation_fun, threshold_ms \\ 1000) do
    {result, execution_time} =
      measure_operation(
        operation_name,
        fn ->
          operation_fun.(session)
        end,
        threshold_ms
      )

    {session, result, execution_time}
  end

  @spec measure_page_load(any(), any(), any()) :: {any(), any(), any()}
  @doc """
  Measures page load time and logs the result.

  ## Parameters
  - session: Wallaby session
  - page_name: Name of the page being loaded
  - threshold_ms: Warning threshold in milliseconds

  ## Returns
  {session, load_time_ms}
  """
  def measure_page_load(session, page_name, threshold_ms \\ 2000) do
    {session, _result, load_time} =
      measure_session_operation(
        session,
        "Page load: #{page_name}",
        fn session ->
          start_time = System.monotonic_time(:millisecond)
          _session = SpacecastWeb.TestHelpers.WallabyUIHelper.wait_for_live_view(session)
          end_time = System.monotonic_time(:millisecond)
          {session, end_time - start_time}
        end,
        threshold_ms
      )

    {session, load_time}
  end

  @doc """
  Measures form submission time and logs the result.

  ## Parameters
  - session: Wallaby session
  - form_name: Name of the form being submitted
  - form_data: Data to submit
  - submit_button: Submit button text
  - threshold_ms: Warning threshold in milliseconds

  ## Returns
  {session, submission_time_ms}
  """
  def measure_form_submission(session, form_name, form_data, submit_button, threshold_ms \\ 3000) do
    {session, _result, submission_time} =
      measure_session_operation(
        session,
        "Form submission: #{form_name}",
        fn session ->
          start_time = System.monotonic_time(:millisecond)

          session =
            SpacecastWeb.TestHelpers.WallabyUIHelper.fill_and_submit_form(
              session,
              form_data,
              submit_button
            )

          end_time = System.monotonic_time(:millisecond)
          {session, end_time - start_time}
        end,
        threshold_ms
      )

    {session, submission_time}
  end

  @doc """
  Measures flash message wait time and logs the result.

  ## Parameters
  - session: Wallaby session
  - message_type: Type of flash message
  - message_text: Expected message text
  - threshold_ms: Warning threshold in milliseconds

  ## Returns
  {session, wait_time_ms}
  """
  def measure_flash_wait(session, message_type, message_text, threshold_ms \\ 2000) do
    {session, _result, wait_time} =
      measure_session_operation(
        session,
        "Flash message wait: #{message_type} - #{message_text}",
        fn session ->
          start_time = System.monotonic_time(:millisecond)

          session =
            SpacecastWeb.TestHelpers.WallabyUIHelper.wait_for_flash_message(
              session,
              message_type,
              message_text
            )

          end_time = System.monotonic_time(:millisecond)
          {session, end_time - start_time}
        end,
        threshold_ms
      )

    {session, wait_time}
  end

  @doc """
  Creates a performance report for a test run.

  ## Parameters
  - measurements: List of {operation_name, execution_time_ms} tuples

  ## Returns
  String containing the performance report
  """
  def generate_performance_report(measurements) do
    total_time = Enum.reduce(measurements, 0, fn {_name, time}, acc -> acc + time end)
    avg_time = total_time / length(measurements)
    max_time = Enum.max_by(measurements, fn {_name, time} -> time end)
    min_time = Enum.min_by(measurements, fn {_name, time} -> time end)

    slow_operations = Enum.filter(measurements, fn {_name, time} -> time > 1000 end)

    """
    Performance Report
    =================
    Total Operations: #{length(measurements)}
    Total Time: #{total_time}ms
    Average Time: #{Float.round(avg_time, 2)}ms
    Fastest Operation: #{elem(min_time, 0)} (#{elem(min_time, 1)}ms)
    Slowest Operation: #{elem(max_time, 0)} (#{elem(max_time, 1)}ms)

    Slow Operations (>1000ms):
    #{Enum.map_join(slow_operations, "\n", fn {name, time} -> "  - #{name}: #{time}ms" end)}
    """
  end

  @doc """
  Logs performance information with appropriate warning levels.

  ## Parameters
  - operation_name: Name of the operation
  - execution_time: Execution time in milliseconds
  - threshold_ms: Warning threshold
  """
  def log_performance(operation_name, execution_time, threshold_ms) do
    cond do
      execution_time > threshold_ms * 2 ->
        IO.puts("🚨 SLOW: #{operation_name} took #{execution_time}ms (threshold: #{threshold_ms}ms)")

      execution_time > threshold_ms ->
        IO.puts("⚠️  WARNING: #{operation_name} took #{execution_time}ms (threshold: #{threshold_ms}ms)")

      true ->
        IO.puts("✅ #{operation_name} took #{execution_time}ms")
    end
  end

  @doc """
  Measures database operation performance.

  ## Parameters
  - operation_name: Name of the database operation
  - operation_fun: Function to measure
  - threshold_ms: Warning threshold in milliseconds

  ## Returns
  {result, execution_time_ms}
  """
  def measure_db_operation(operation_name, operation_fun, threshold_ms \\ 500) do
    measure_operation("DB: #{operation_name}", operation_fun, threshold_ms)
  end

  @doc """
  Creates a performance benchmark for common operations.

  ## Parameters
  - session: Wallaby session

  ## Returns
  Map containing benchmark results
  """
  def run_performance_benchmark(session) do
    measurements = []

    # Benchmark page loads
    {session, page_load_time} = measure_page_load(session, "Resources Dashboard")
    measurements = [{:page_load, page_load_time} | measurements]

    # Benchmark form operations
    _session = visit(session, "/resources/new")

    {session, form_submission_time} =
      measure_form_submission(
        session,
        "Resource Creation",
        %{"resource[name]" => "Benchmark Resource"},
        "Create Resource"
      )

    measurements = [{:form_submission, form_submission_time} | measurements]

    # Benchmark flash message wait
    {session, flash_wait_time} =
      measure_flash_wait(
        session,
        "info",
        "Resource created successfully"
      )

    measurements = [{:flash_wait, flash_wait_time} | measurements]

    # Generate report
    report = generate_performance_report(measurements)
    IO.puts(report)

    {session, measurements}
  end
end
