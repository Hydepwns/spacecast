defmodule SpacecastWeb.OfflineController do
  use SpacecastWeb, :controller

  @doc """
  Renders the offline page.
  Phoenix controller action: renders the offline fallback page without the default layout.
  """
  def index(conn, _params) do
    render(conn, :render, layout: false)
  end
end
