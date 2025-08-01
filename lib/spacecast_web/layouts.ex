defmodule SpacecastWeb.Layouts do
  use Phoenix.Component

  use Phoenix.Template,
    root: "lib/spacecast_web",
    namespace: SpacecastWeb

  import SpacecastWeb.Components.Common.ThemeToggle, only: [theme_toggle: 1]
  import SpacecastWeb.Components.UI.DebugGrid, only: [debug_grid: 1]
  import Phoenix.Controller, only: [get_csrf_token: 0]

  use Phoenix.VerifiedRoutes,
    endpoint: SpacecastWeb.Endpoint,
    router: SpacecastWeb.Router,
    statics: SpacecastWeb.static_paths()

  embed_templates "layouts/*"
end
