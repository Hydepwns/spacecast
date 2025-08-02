defmodule Spacecast.TestSupport.ResourceSystemHelper do
  @moduledoc """
  Helper functions for setting up the resource system in tests.
  """

  alias Spacecast.Resources.ResourceSystem

  @doc """
  Sets up the resource system for tests.
  This ensures the ResourceSystem is available and the store is reset.
  """
  def setup_resource_system do
    # Ensure we have proper database connection ownership
    Ecto.Adapters.SQL.Sandbox.checkout(Spacecast.Repo)

    # Reset the store and cache to ensure a clean state
    # Only reset if we're in a test environment and have proper connection
    try do
      ResourceSystem.reset_store()
      ResourceSystem.reset_cache()
    rescue
      DBConnection.OwnershipError ->
        # If we don't have proper ownership, just reset the cache
        # The store will be reset by the test setup
        ResourceSystem.reset_cache()
    end
  end
end
