defmodule SpacecastWeb.ServiceWorkerController do
  use SpacecastWeb, :controller

  def index(conn, _params) do
    conn
    |> put_resp_content_type("application/javascript")
    |> put_resp_header("cache-control", "no-cache")
    |> send_file(
      200,
      Application.app_dir(:spacecast, "priv/static/assets/service-worker.js")
    )
  end
end
