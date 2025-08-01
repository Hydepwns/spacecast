defmodule SpacecastWeb.Plugs.SandboxSession do
  @moduledoc """
  Plug to ensure sandbox information is available in the session for websocket connections.
  """

  import Plug.Conn

  @doc """
  Ensures that if a sandbox cookie is present, it's also stored in the session
  so that websocket connections can access it.
  """
  def init(opts), do: opts

  def call(conn, _opts) do
    if Application.get_env(:spacecast, :sql_sandbox, false) do
      # First fetch the session to ensure it's available
      conn = fetch_session(conn)

      # Check if we have a sandbox cookie
      case get_req_header(conn, "cookie") do
        [cookie_header] ->
          # Parse the cookie header to find the sandbox cookie
          cookies = Plug.Conn.Cookies.decode(cookie_header)

          case Map.get(cookies, "_phoenix_liveview_sandbox") do
            nil -> conn
            sandbox_pid ->
              # Store the sandbox PID in the session
              conn
              |> put_session("_phoenix_liveview_sandbox", sandbox_pid)
          end

        _ -> conn
      end
    else
      conn
    end
  end
end
