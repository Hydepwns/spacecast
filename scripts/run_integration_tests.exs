#!/usr/bin/env elixir

# Integration Test Runner for Spacecast LiveView
#
# This script provides comprehensive integration testing capabilities including:
# - API integration tests
# - Real-time integration tests
# - Event-driven architecture tests
# - External service integration tests
# - Performance and load tests
# - Security integration tests
#
# Usage:
#   elixir scripts/run_integration_tests.exs [options]
#
# Options:
#   -t, --test-type TYPE     Run specific test type (api, realtime, events, external, performance, security, all)
#   -p, --pattern PATTERN    Run tests matching pattern
#   -d, --debug              Enable debug mode with detailed output
#   -P, --parallel           Run tests in parallel
#   -T, --timeout TIMEOUT    Set test timeout in milliseconds (default: 30000)
#   -r, --report             Generate detailed test report
#   -m, --memory             Monitor memory usage
#   -c, --coverage           Generate coverage report
#   -h, --help               Show this help message

defmodule IntegrationTestRunner do
  @moduledoc """
  Comprehensive integration test runner for Spacecast LiveView application.
  """

  def main(args \\ []) do
    {opts, _} =
      OptionParser.parse!(args,
        strict: [
          test_type: :string,
          pattern: :string,
          debug: :boolean,
          parallel: :boolean,
          timeout: :integer,
          report: :boolean,
          memory: :boolean,
          coverage: :boolean,
          help: :boolean
        ],
        aliases: [
          t: :test_type,
          p: :pattern,
          d: :debug,
          P: :parallel,
          T: :timeout,
          r: :report,
          m: :memory,
          c: :coverage,
          h: :help
        ]
      )

    if opts[:help] do
      show_help()
      System.halt(0)
    end

    # Set default options
    opts =
      Keyword.merge(
        [
          test_type: "all",
          pattern: nil,
          debug: false,
          parallel: false,
          timeout: 30000,
          report: false,
          memory: false,
          coverage: false
        ],
        opts
      )

    # Start the application
    Application.ensure_all_started(:spacecast)

    # Run tests
    run_tests(opts)
  end

  defp run_tests(opts) do
    IO.puts("\n🚀 Starting Integration Test Suite")
    IO.puts("=" <> String.duplicate("=", 50))

    start_time = System.monotonic_time(:millisecond)

    # Monitor memory if requested
    if opts[:memory] do
      start_memory_monitoring()
    end

    # Determine test files to run
    test_files = get_test_files(opts[:test_type], opts[:pattern])

    if Enum.empty?(test_files) do
      IO.puts("❌ No test files found matching criteria")
      System.halt(1)
    end

    IO.puts("📋 Found #{length(test_files)} test file(s) to run")
    Enum.each(test_files, fn file -> IO.puts("   - #{file}") end)
    IO.puts("")

    # Run tests
    results =
      if opts[:parallel] do
        run_tests_parallel(test_files, opts)
      else
        run_tests_sequential(test_files, opts)
      end

    end_time = System.monotonic_time(:millisecond)
    total_time = end_time - start_time

    # Generate report
    generate_report(results, total_time, opts)

    # Stop memory monitoring
    if opts[:memory] do
      stop_memory_monitoring()
    end

    # Exit with appropriate code
    if Enum.any?(results, fn {_file, result} -> result.status == :failed end) do
      System.halt(1)
    else
      System.halt(0)
    end
  end

  defp get_test_files(test_type, pattern) do
    base_path = "test/spacecast_web/integration"

    case test_type do
      "api" ->
        find_test_files(base_path, "api_integration_test.exs", pattern)

      "realtime" ->
        find_test_files(base_path, "realtime_integration_test.exs", pattern)

      "events" ->
        find_test_files(base_path, "event_driven_integration_test.exs", pattern)

      "external" ->
        find_test_files(base_path, "external_service_integration_test.exs", pattern)

      "performance" ->
        find_test_files(base_path, "performance_integration_test.exs", pattern)

      "security" ->
        find_test_files(base_path, "security_integration_test.exs", pattern)

      "all" ->
        find_all_test_files(base_path, pattern)

      _ ->
        IO.puts("❌ Unknown test type: #{test_type}")
        []
    end
  end

  defp find_test_files(base_path, filename, pattern) do
    file_path = Path.join(base_path, filename)

    if File.exists?(file_path) do
      if pattern do
        if String.contains?(filename, pattern) do
          [file_path]
        else
          []
        end
      else
        [file_path]
      end
    else
      []
    end
  end

  defp find_all_test_files(base_path, pattern) do
    case File.ls(base_path) do
      {:ok, files} ->
        files
        |> Enum.filter(&String.ends_with?(&1, "_integration_test.exs"))
        |> Enum.map(&Path.join(base_path, &1))
        |> Enum.filter(fn file ->
          if pattern do
            String.contains?(Path.basename(file), pattern)
          else
            true
          end
        end)

      {:error, _} ->
        []
    end
  end

  defp run_tests_sequential(test_files, opts) do
    Enum.map(test_files, fn file ->
      run_single_test_file(file, opts)
    end)
  end

  defp run_tests_parallel(test_files, opts) do
    test_files
    |> Enum.map(fn file ->
      Task.async(fn -> run_single_test_file(file, opts) end)
    end)
    |> Task.await_many()
  end

  defp run_single_test_file(file, opts) do
    IO.puts("🧪 Running #{Path.basename(file)}")

    start_time = System.monotonic_time(:millisecond)

    # Set test timeout
    if opts[:timeout] do
      Process.put(:test_timeout, opts[:timeout])
    end

    # Run the test file
    result =
      case run_test_file(file, opts) do
        {:ok, output} ->
          end_time = System.monotonic_time(:millisecond)
          duration = end_time - start_time

          {file,
           %{
             status: :passed,
             duration: duration,
             output: output
           }}

        {:error, reason} ->
          end_time = System.monotonic_time(:millisecond)
          duration = end_time - start_time

          {file,
           %{
             status: :failed,
             duration: duration,
             error: reason
           }}
      end

    # Print result
    case result do
      {_file, %{status: :passed, duration: duration}} ->
        IO.puts("✅ #{Path.basename(file)} passed in #{duration}ms")

      {_file, %{status: :failed, duration: duration, error: error}} ->
        IO.puts("❌ #{Path.basename(file)} failed in #{duration}ms")

        if opts[:debug] do
          IO.puts("   Error: #{inspect(error)}")
        end
    end

    result
  end

  defp run_test_file(file, opts) do
    # Set up test environment
    setup_test_environment(opts)

    # Run the test using Mix
    cmd = build_mix_test_command(file, opts)

    case System.cmd("mix", cmd, stderr_to_stdout: true) do
      {output, 0} ->
        {:ok, output}

      {output, exit_code} ->
        {:error, "Exit code #{exit_code}: #{output}"}
    end
  end

  defp build_mix_test_command(file, opts) do
    cmd = ["test", file]

    cmd =
      if opts[:debug] do
        cmd ++ ["--trace"]
      else
        cmd
      end

    cmd =
      if opts[:coverage] do
        cmd ++ ["--cover"]
      else
        cmd
      end

    cmd
  end

  defp setup_test_environment(opts) do
    # Set environment variables for testing
    System.put_env("MIX_ENV", "test")

    if opts[:debug] do
      System.put_env("DEBUG", "true")
    end

    # Reset test database
    reset_test_database()
  end

  defp reset_test_database do
    # Reset the test database
    System.cmd("mix", ["ecto.reset", "--quiet"], stderr_to_stdout: true)
  end

  defp generate_report(results, total_time, opts) do
    IO.puts("\n📊 Test Results Summary")
    IO.puts("=" <> String.duplicate("=", 50))

    # Calculate statistics
    total_tests = length(results)
    passed_tests = Enum.count(results, fn {_file, result} -> result.status == :passed end)
    failed_tests = total_tests - passed_tests
    success_rate = if total_tests > 0, do: passed_tests / total_tests * 100, else: 0

    # Print summary
    IO.puts("Total Tests: #{total_tests}")
    IO.puts("Passed: #{passed_tests}")
    IO.puts("Failed: #{failed_tests}")
    IO.puts("Success Rate: #{Float.round(success_rate, 2)}%")
    IO.puts("Total Time: #{total_time}ms")

    # Print detailed results
    if opts[:debug] or failed_tests > 0 do
      IO.puts("\n📋 Detailed Results:")

      Enum.each(results, fn {file, result} ->
        case result do
          %{status: :passed, duration: duration} ->
            IO.puts("✅ #{Path.basename(file)} (#{duration}ms)")

          %{status: :failed, duration: duration, error: error} ->
            IO.puts("❌ #{Path.basename(file)} (#{duration}ms)")
            IO.puts("   Error: #{error}")
        end
      end)
    end

    # Generate detailed report if requested
    if opts[:report] do
      generate_detailed_report(results, total_time)
    end

    # Performance analysis
    if opts[:memory] do
      analyze_performance(results, total_time)
    end
  end

  defp generate_detailed_report(results, total_time) do
    report_file = "tmp/integration_test_report_#{DateTime.utc_now() |> DateTime.to_unix()}.json"

    report_data = %{
      timestamp: DateTime.utc_now() |> DateTime.to_iso8601(),
      total_time: total_time,
      results:
        Enum.map(results, fn {file, result} ->
          %{
            file: Path.basename(file),
            status: result.status,
            duration: result.duration,
            error: Map.get(result, :error)
          }
        end),
      summary: %{
        total: length(results),
        passed: Enum.count(results, fn {_file, result} -> result.status == :passed end),
        failed: Enum.count(results, fn {_file, result} -> result.status == :failed end)
      }
    }

    # Ensure tmp directory exists
    File.mkdir_p!("tmp")

    # Write report
    File.write!(report_file, Jason.encode!(report_data, pretty: true))
    IO.puts("📄 Detailed report saved to: #{report_file}")
  end

  defp analyze_performance(results, total_time) do
    IO.puts("\n⚡ Performance Analysis")
    IO.puts("-" <> String.duplicate("-", 30))

    # Calculate performance metrics
    durations = Enum.map(results, fn {_file, result} -> result.duration end)
    avg_duration = Enum.sum(durations) / length(durations)
    max_duration = Enum.max(durations)
    min_duration = Enum.min(durations)

    IO.puts("Average Test Duration: #{Float.round(avg_duration, 2)}ms")
    IO.puts("Fastest Test: #{min_duration}ms")
    IO.puts("Slowest Test: #{max_duration}ms")
    IO.puts("Total Suite Time: #{total_time}ms")

    # Performance recommendations
    if avg_duration > 5000 do
      IO.puts("⚠️  Warning: Average test duration is high. Consider optimization.")
    end

    if max_duration > 30000 do
      IO.puts("⚠️  Warning: Some tests are taking too long. Review slow tests.")
    end
  end

  defp start_memory_monitoring do
    # Start memory monitoring process
    spawn(fn -> monitor_memory() end)
  end

  defp stop_memory_monitoring do
    # Stop memory monitoring
    # This would be implemented with a proper monitoring system
  end

  defp monitor_memory do
    # Monitor memory usage during tests
    # This is a simplified implementation
    initial_memory = :erlang.memory(:total)
    IO.puts("💾 Initial memory usage: #{initial_memory} bytes")

    # Monitor for a reasonable time
    Process.sleep(1000)

    final_memory = :erlang.memory(:total)
    memory_diff = final_memory - initial_memory

    # 50MB
    if memory_diff > 50 * 1024 * 1024 do
      IO.puts("⚠️  Warning: Memory usage increased by #{memory_diff} bytes")
    end
  end

  defp show_help do
    IO.puts("""
    Integration Test Runner for Spacecast LiveView

    Usage:
      elixir scripts/run_integration_tests.exs [options]

    Options:
      -t, --test-type TYPE     Run specific test type:
                                - api: API integration tests
                                - realtime: Real-time integration tests
                                - events: Event-driven architecture tests
                                - external: External service integration tests
                                - performance: Performance and load tests
                                - security: Security integration tests
                                - all: All integration tests (default)

      -p, --pattern PATTERN    Run tests matching pattern
      -d, --debug              Enable debug mode with detailed output
      -P, --parallel           Run tests in parallel
      -T, --timeout TIMEOUT    Set test timeout in milliseconds (default: 30000)
      -r, --report             Generate detailed test report
      -m, --memory             Monitor memory usage
      -c, --coverage           Generate coverage report
      -h, --help               Show this help message

    Examples:
      # Run all integration tests
      elixir scripts/run_integration_tests.exs

      # Run only API integration tests
      elixir scripts/run_integration_tests.exs -t api

      # Run security tests with debug output
      elixir scripts/run_integration_tests.exs -t security -d

      # Run performance tests in parallel with coverage
      elixir scripts/run_integration_tests.exs -t performance -P -c

      # Run tests matching pattern with detailed report
      elixir scripts/run_integration_tests.exs -p "api" -r -m
    """)
  end
end

# Run the script
IntegrationTestRunner.main(System.argv())
