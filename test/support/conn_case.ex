defmodule SpacecastWeb.ConnCase do
  @moduledoc """
  This module defines the test case to be used by
  tests that require setting up a connection.

  Such tests rely on `Phoenix.ConnTest` and also
  import other functionality to make it easier
  to build common data structures and query the data layer.

  Finally, if the test case interacts with the database,
  we enable the SQL sandbox, so changes done to the database
  are reverted at the end of every test. If you are using
  PostgreSQL, you can even run database tests asynchronously
  by setting `use SpacecastWeb.ConnCase, async: true`, although
  this option is not recommended for other databases.
  """

  use ExUnit.CaseTemplate

  using do
    quote do
      import Plug.Conn
      import Phoenix.ConnTest
      import SpacecastWeb.ConnCase
      import Phoenix.Component
      import Phoenix.VerifiedRoutes

      # Ensure Router and its Helpers are compiled and available
      require SpacecastWeb.Router
      alias SpacecastWeb.Router.Helpers, as: Routes
      @phoenix_router SpacecastWeb.Router

      # The default endpoint for testing
      @endpoint SpacecastWeb.Endpoint

      # Add test routes
      setup do
        # Configure test routes
        Application.put_env(:spacecast, SpacecastWeb.Router,
          live_routes: [
            {"/test-types", SpacecastWeb.TestTypeLive},
            {"/", SpacecastWeb.HomeLive}
          ]
        )

        :ok
      end
    end
  end

  setup tags do
    # Use manual sandbox mode for all tests to ensure proper connection management
    pid = Spacecast.DataCase.setup_sandbox(tags)

    # Always allow the current process for database access
    Ecto.Adapters.SQL.Sandbox.allow(Spacecast.Repo, self(), pid)

    {:ok, conn: Phoenix.ConnTest.build_conn()}
  end
end
