defmodule Spacecast.TestSupport.ThemeSystemHelper do
  @moduledoc """
  Helper functions for setting up the theme system with per-test isolation.
  """

  import ExUnit.Callbacks, only: [on_exit: 1]

  @doc """
  Sets up a unique ETS table for the current test process.
  This ensures that each test has its own isolated theme storage.
  """
  def setup_theme_system_isolation do
    # Create a unique table name for this test process
    table = :"theme_system_themes_test_#{System.unique_integer([:positive])}"

    # Store the table name in the process dictionary
    Process.put(:theme_system_ets_table, table)

    # Clean up the table when the test process exits
    on_exit(fn ->
      try do
        if :ets.info(table), do: :ets.delete(table)
      rescue
        _ -> :ok
      end
    end)

    # Initialize the table with the theme system
    Spacecast.ThemeSystem.reset_themes()

    # Ensure the table is created and accessible
    ensure_ets_table_exists(table)

    {:ok, table}
  end

  defp ensure_ets_table_exists(table) do
    case :ets.info(table) do
      :undefined ->
        :ets.new(table, [:named_table, :public, :set])
        :ets.insert(table, {:next_id, 1})

      _ ->
        :ok
    end
  end

  @doc """
  Cleans up the theme system for the current test process.
  """
  def cleanup_theme_system do
    table = Process.get(:theme_system_ets_table)

    try do
      if table && :ets.info(table), do: :ets.delete(table)
    rescue
      _ -> :ok
    end

    Process.delete(:theme_system_ets_table)
  end
end
