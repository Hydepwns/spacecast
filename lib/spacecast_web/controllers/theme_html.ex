defmodule SpacecastWeb.ThemeHTML do
  @moduledoc """
  Handles HTML template rendering for themes in Phoenix 1.7+.
  """
  use SpacecastWeb, :html

  # Phoenix 1.7+ automatically generates render functions for templates
  # in lib/spacecast_web/templates/theme/
  # No additional render functions needed here

  embed_templates "theme_html/*"

  def index(assigns) do
    ~H"""
    <div class="container mx-auto px-4 py-8">
      <h1 class="text-3xl font-bold mb-6">Themes</h1>
      <p class="text-gray-600 mb-4">Theme management has been moved to LiveView for better interactivity.</p>
      <a href="/themes/live" class="bg-blue-500 hover:bg-blue-700 text-white font-bold py-2 px-4 rounded">
        Go to Live Theme Manager
      </a>
    </div>
    """
  end
end
