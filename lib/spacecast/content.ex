defmodule Spacecast.Content do
  @moduledoc """
  Handles content-related functionality, including table of contents data.
  """

  @doc """
  Returns the table of contents data for the application.
  """
  def get_toc_data do
    [
      {"ascii-banner", "Welcome to Spacecast"},
      {"introduction", "Introduction"},
      {"ascii-examples", "ASCII Art Examples"},
      {"lists", "Lists"},
      {"tables", "Tables"},
      {"forms", "Forms"},
      {"grids", "Grids"},
      {"features", "Features"},
      {"discussion", "Discussion"}
    ]
  end
end
