defmodule SpacecastWeb.Router.ExampleRoutes do
  @moduledoc """
  Example and playground routes for Spacecast.

  This module contains routes for interactive examples and playground features.
  """

  defmacro __using__(_opts) do
    quote do
      # Examples routes
      scope "/examples", Examples, as: :examples do
        live("/", ExamplesIndexLive, :index, as: :index)
        live("/type-validation", TypeValidationExample, :index, as: :type_validation)
        live("/resource-assigns", UserResourceLive, :index, as: :resource_assigns)
        live("/user-resource", UserResourceExampleLive, :index, as: :user_resource)
        live("/ecto-resource", EctoResourceExampleLive, :index, as: :ecto_resource)
        live("/change-tracking", ChangeTrackingExampleLive, :index, as: :change_tracking)
        live("/nested-validation", NestedValidationExampleLive, :index, as: :nested_validation)
        live("/transformation", TransformationExampleLive, :index, as: :transformation)
        live("/transformation-metrics", TransformationMetricsLive, :index, as: :transformation_metrics)

        live("/context-validation-tracking", ContextValidationTrackingExampleLive, :index,
          as: :context_validation_tracking
        )

        live("/event-system", EventSystemExampleLive, :index, as: :event_system)

        # Interactive examples
        live("/ascii-art-editor", AsciiArtEditorLive, :index, as: :ascii_art_editor)
        live("/system-monitor", SystemMonitorLive, :index, as: :system_monitor)
        live("/code-flow-visualizer", CodeFlowVisualizerLive, :index, as: :code_flow_visualizer)
        live("/network-topology", NetworkTopologyLive, :index, as: :network_topology)
        live("/data-visualizer", DataVisualizerLive, :index, as: :data_visualizer)
      end

      # Playground routes
      scope "/playground", Live.Playground, as: :playground do
        # Terminal demo route removed
      end
    end
  end
end
