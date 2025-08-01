defmodule SpacecastWeb.LiveViewCase do
  @moduledoc """
  This module defines the test case to be used by
  tests that require setting up a LiveView connection.

  Such tests rely on `Phoenix.LiveViewTest` and also
  import other functionality to make it easier
  to build common data structures and query the data layer.

  This case is specifically designed for LiveView tests
  with proper database sandbox handling.
  """

  use ExUnit.CaseTemplate

  using do
    quote do
      import Plug.Conn
      import Phoenix.ConnTest
      import Phoenix.LiveViewTest
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
    # Manual sandbox mode for LiveView tests
    pid = Spacecast.DataCase.setup_sandbox(tags)

    # Allow current process and any spawned processes
    Ecto.Adapters.SQL.Sandbox.allow(Spacecast.Repo, self(), pid)

    {:ok, conn: Phoenix.ConnTest.build_conn()}
  end
end
