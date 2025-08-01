#!/usr/bin/env elixir

# Test Runner for Stabilized Test Suite
# This script helps run both Wallaby and LiveView tests with proper isolation

# This script should be run from the project root where mix.exs is located

defmodule TestRunner do
  @moduledoc """
  Test runner for the stabilized test suite.
  
  This script provides utilities for running both Wallaby and LiveView tests
  with proper isolation and debugging capabilities.
  """

  def main do
    # Parse command line arguments
    {opts, _args, _invalid} = OptionParser.parse(
      System.argv(),
      strict: [
        type: :string,
        pattern: :string,
        debug: :boolean,
        parallel: :boolean,
        timeout: :integer
      ],
      aliases: [
        t: :type,
        p: :pattern,
        d: :debug,
        P: :parallel,
        T: :timeout
      ]
    )

    # Set default values
    test_type = Keyword.get(opts, :type, "all")
    pattern = Keyword.get(opts, :pattern, "")
    debug = Keyword.get(opts, :debug, false)
    parallel = Keyword.get(opts, :parallel, false)
    timeout = Keyword.get(opts, :timeout, 30000)

    # Set environment variables for test configuration
    System.put_env("WALLABY_DEBUG", if(debug, do: "true", else: "false"))
    System.put_env("TEST_TIMEOUT", to_string(timeout))

    case test_type do
      "wallaby" -> run_wallaby_tests(pattern, debug, parallel)
      "liveview" -> run_liveview_tests(pattern, debug, parallel)
      "all" -> run_all_tests(pattern, debug, parallel)
      _ -> print_usage()
    end
  end

  defp run_wallaby_tests(pattern, debug, parallel) do
    IO.puts("🧪 Running Wallaby tests...")
    
    test_files = find_test_files("test/**/features/*_test.exs", pattern)
    
    if debug do
      IO.puts("Debug mode enabled - screenshots and HTML will be saved to tmp/")
      File.mkdir_p!("tmp")
    end

    run_tests(test_files, parallel, "Wallaby")
  end

  defp run_liveview_tests(pattern, _debug, parallel) do
    IO.puts("⚡ Running LiveView tests...")
    
    test_files = find_test_files("test/**/live/*_test.exs", pattern)
    
    run_tests(test_files, parallel, "LiveView")
  end

  defp run_all_tests(pattern, debug, parallel) do
    IO.puts("🚀 Running all tests...")
    
    # Run LiveView tests first (faster and more reliable)
    run_liveview_tests(pattern, debug, parallel)
    
    # Then run Wallaby tests
    run_wallaby_tests(pattern, debug, parallel)
  end

  defp find_test_files(glob_pattern, pattern) do
    Path.wildcard(glob_pattern)
    |> Enum.filter(fn file ->
      if pattern == "" do
        true
      else
        String.contains?(file, pattern)
      end
    end)
    |> Enum.sort()
  end

  defp run_tests(test_files, parallel, test_type) do
    if test_files == [] do
      IO.puts("No test files found matching the pattern.")
      :ok
    else
      IO.puts("Found #{length(test_files)} test files:")
      Enum.each(test_files, &IO.puts("  - #{&1}"))

      # Build the mix test command
      cmd = build_test_command(test_files, parallel)
      
      IO.puts("\nRunning: mix test #{Enum.join(cmd, " ")}")
      IO.puts("=" <> String.duplicate("=", 50))

      # Run the tests
      {output, exit_code} = System.cmd("mix", ["test"] ++ cmd, 
        env: [{"MIX_ENV", "test"}],
        stderr_to_stdout: true
      )

      IO.puts(output)

      case exit_code do
        0 -> 
          IO.puts("\n✅ #{test_type} tests passed!")
          :ok
        _ -> 
          IO.puts("\n❌ #{test_type} tests failed with exit code #{exit_code}")
          :error
      end
    end
  end

  defp build_test_command(test_files, parallel) do
    cmd = []
    
    # Add test files
    cmd = cmd ++ test_files
    
    # Add parallel flag if requested
    if parallel do
      cmd = cmd ++ ["--max-failures", "5"]
    end
    
    # Add timeout
    timeout = System.get_env("TEST_TIMEOUT") || "30000"
    cmd = cmd ++ ["--timeout", timeout]
    
    # Add trace for debugging
    if System.get_env("WALLABY_DEBUG") == "true" do
      cmd = cmd ++ ["--trace"]
    end
    
    cmd
  end

  defp print_usage do
    IO.puts("""
    Test Runner for Stabilized Test Suite
    
    Usage:
      elixir scripts/run_stabilized_tests.exs [options]
    
    Options:
      -t, --type TYPE        Test type: wallaby, liveview, or all (default: all)
      -p, --pattern PATTERN  Filter test files by pattern
      -d, --debug           Enable debug mode (screenshots, HTML dumps)
      -P, --parallel        Run tests in parallel
      -T, --timeout MS      Set test timeout in milliseconds (default: 30000)
    
    Examples:
      # Run all tests
      elixir scripts/run_stabilized_tests.exs
      
      # Run only Wallaby tests with debug
      elixir scripts/run_stabilized_tests.exs -t wallaby -d
      
      # Run LiveView tests matching "relationship"
      elixir scripts/run_stabilized_tests.exs -t liveview -p relationship
      
      # Run all tests in parallel with custom timeout
      elixir scripts/run_stabilized_tests.exs -P -T 60000
    """)
  end
end

# Run the test runner
TestRunner.main() 