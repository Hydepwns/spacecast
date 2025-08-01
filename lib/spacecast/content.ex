defmodule Spacecast.Content do
  @moduledoc """
  Handles content-related functionality, including table of contents data.
  """

  @doc """
  Returns the table of contents data for the application.
  """
  def get_toc_data do
    [
      %{
        title: "Getting Started",
        items: [
          %{title: "Introduction", path: "/docs/introduction"},
          %{title: "Installation", path: "/docs/installation"},
          %{title: "Quick Start", path: "/docs/quick-start"}
        ]
      },
      %{
        title: "Core Concepts",
        items: [
          %{title: "Resources", path: "/docs/resources"},
          %{title: "Events", path: "/docs/events"},
          %{title: "Validation", path: "/docs/validation"}
        ]
      },
      %{
        title: "Advanced Topics",
        items: [
          %{title: "Event Sourcing", path: "/docs/event-sourcing"},
          %{title: "Resource Integration", path: "/docs/resource-integration"},
          %{title: "Custom Validations", path: "/docs/custom-validations"}
        ]
      }
    ]
  end
end
