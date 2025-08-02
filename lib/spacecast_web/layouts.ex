defmodule SpacecastWeb.Layouts do
  use Phoenix.Component

  use Phoenix.Template,
    root: "lib/spacecast_web",
    namespace: SpacecastWeb

  import SpacecastWeb.Components.Common.FloatingControls, only: [floating_controls: 1]
  import Phoenix.Controller, only: [get_csrf_token: 0]

  use Phoenix.VerifiedRoutes,
    endpoint: SpacecastWeb.Endpoint,
    router: SpacecastWeb.Router,
    statics: SpacecastWeb.static_paths()

  embed_templates("layouts/*")
end
