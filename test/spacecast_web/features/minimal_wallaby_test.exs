defmodule SpacecastWeb.MinimalWallabyTest do
  use SpacecastWeb.WallabyCase, async: false

  setup _context do
    # Create a mock session since we're not using Wallaby.Feature
    mock_session = %{
      driver: %{mock: true},
      server: %{mock: true, pid: self()},
      session_id: "mock-session-#{System.unique_integer()}",
      mock: true,
      type: :session
    }

    {:ok, session: mock_session}
  end

  feature "home page loads", %{session: session} do
    session = visit(session, "/")
    assert has_text?(session, "Spacecast")
  end
end
