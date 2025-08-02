Mox.set_mox_global(false)
ExUnit.start()
{:ok, _} = Application.ensure_all_started(:mox)
{:ok, _} = Application.ensure_all_started(:spacecast)

# Load test support files
Code.require_file("test/support/signal_nif_stub.ex")

# Ensure the ETS table for mock resources exists and is public
if :ets.whereis(:mock_resources) == :undefined do
  :ets.new(:mock_resources, [:set, :public, :named_table])
end

# Ensure the theme system ETS table exists for all tests
if :ets.whereis(:theme_system_default) == :undefined do
  :ets.new(:theme_system_default, [:set, :public, :named_table])
end

# Skip database setup for tests that don't need it
# Ecto.Adapters.SQL.Sandbox.mode(Spacecast.Repo, :manual)

# Start Wallaby for browser tests only if chromedriver is available
case System.find_executable("chromedriver") do
  nil ->
    IO.puts("⚠️  Chromedriver not found - skipping Wallaby startup")
    IO.puts("   Browser tests will be skipped. Run 'nix-shell' and './scripts/test_browser.sh' for browser tests.")
    # Set environment variable to indicate Wallaby should be skipped
    System.put_env("WALLABY_SKIP", "true")

  _chromedriver_path ->
    IO.puts("✅ Chromedriver found - starting Wallaby")

    case Application.ensure_all_started(:wallaby) do
      {:ok, _} ->
        IO.puts("✅ Wallaby started successfully")
        :ok

      {:error, reason} ->
        IO.puts("⚠️  Failed to start Wallaby: #{inspect(reason)}")
        IO.puts("   Browser tests will be skipped.")
        System.put_env("WALLABY_SKIP", "true")
    end
end

# Start the resource system
{:ok, _} =
  case Spacecast.Resources.ResourceSystem.start_link() do
    {:ok, pid} -> {:ok, pid}
    {:error, {:already_started, _pid}} -> {:ok, :already_started}
  end

# Start MockEventStore globally for all tests
case Spacecast.TestSupport.MockEventStore.start_link([]) do
  {:ok, _pid} -> :ok
  {:error, {:already_started, _pid}} -> :ok
end

# Start EventMonitor globally for all tests with TestEventStore
case Spacecast.Events.Core.EventMonitor.start_link(event_store: Spacecast.Events.TestEventStore) do
  {:ok, _pid} -> :ok
  {:error, {:already_started, _pid}} -> :ok
end

# Define setup callback for all tests
defmodule Spacecast.TestSetup do
  use ExUnit.CaseTemplate

  setup do
    # Skip database setup for tests that don't need it
    # Ecto.Adapters.SQL.Sandbox.checkout(Spacecast.Repo)

    # Reset the store safely
    try do
      Spacecast.Resources.ResourceSystem.reset_store()
    rescue
      DBConnection.OwnershipError ->
        # If we don't have proper ownership, skip the reset
        # The database will be cleaned up by the test framework
        :ok
    end

    # Ensure theme system has a default theme for tests
    Spacecast.ThemeSystem.ensure_default_theme()

    # Allow the MockEventStore process to use the test's DB connection
    if Process.whereis(Spacecast.TestSupport.MockEventStore) do
      # Skip database connection for MockEventStore
      # Ecto.Adapters.SQL.Sandbox.allow(Spacecast.Repo, self(), Process.whereis(Spacecast.TestSupport.MockEventStore))
    end

    :ok
  end
end
