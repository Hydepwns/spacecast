defmodule SpacecastWeb.Endpoint do
  use Phoenix.Endpoint, otp_app: :spacecast

  @session_options [
    store: :cookie,
    key: "_spacecast_key",
    signing_salt: "RtEu4ufu",
    same_site: "Lax"
  ]

  socket "/live", SpacecastWeb.Socket,
    websocket: [connect_info: [session: @session_options, cookies: :all]],
    longpoll: [connect_info: [session: @session_options, cookies: :all]]

  plug Plug.Static,
    at: "/",
    from: :spacecast,
    gzip: false,
    only: SpacecastWeb.static_paths()

  if code_reloading? do
    socket "/phoenix/live_reload/socket", Phoenix.LiveReloader.Socket
    plug Phoenix.LiveReloader
    plug Phoenix.CodeReloader
    plug Phoenix.Ecto.CheckRepoStatus, otp_app: :spacecast
  end

  plug Phoenix.LiveDashboard.RequestLogger,
    param_key: "request_logger",
    cookie_key: "request_logger"

  plug Plug.RequestId
  plug Plug.Telemetry, event_prefix: [:phoenix, :endpoint]

  plug Plug.Parsers,
    parsers: [:urlencoded, :multipart, :json],
    pass: ["*/*"],
    json_decoder: Phoenix.json_library()

  plug Plug.MethodOverride
  plug Plug.Head
  plug Plug.Session, @session_options

  plug SpacecastWeb.Plugs.CustomSecurityHeaders

  if Application.compile_env(:spacecast, :sql_sandbox, false) do
    plug Phoenix.Ecto.SQL.Sandbox
    plug SpacecastWeb.Plugs.SandboxSession
  end

  plug SpacecastWeb.Router
end
