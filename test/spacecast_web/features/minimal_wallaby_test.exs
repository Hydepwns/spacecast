defmodule SpacecastWeb.MinimalWallabyTest do
  use SpacecastWeb.WallabyCase, async: false

  feature "home page loads", %{session: session} do
    session = visit(session, "/")
    assert has_text?(session, "Hydepwns")
  end
end
