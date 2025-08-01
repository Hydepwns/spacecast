defmodule SpacecastWeb.PageController do
  use SpacecastWeb, :controller

  @doc """
  Renders the home page.
  Phoenix controller action: renders the home page without the default app layout.
  """
  def home(conn, _params) do
    # The home page is often custom made,
    # so skip the default app layout.
    render(conn, :home, layout: false)
  end

  def favicon(conn, _params) do
    conn
    |> put_resp_content_type("image/x-icon")
    |> send_file(200, "priv/static/favicon.ico")
  end

  def favicon_32(conn, _params) do
    conn
    |> put_resp_content_type("image/png")
    |> send_file(200, "priv/static/images/favicon-32x32.png")
  end

  def favicon_16(conn, _params) do
    conn
    |> put_resp_content_type("image/png")
    |> send_file(200, "priv/static/images/favicon-16x16.png")
  end
end
