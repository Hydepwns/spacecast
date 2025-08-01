defmodule SpacecastWeb.Components.Layout.Layouts do
  use Phoenix.Component

  use Phoenix.Template,
    root: "lib/spacecast_web",
    namespace: SpacecastWeb.Components.Layout

  embed_templates "components/layout/layouts/*"
end
