defmodule SpacecastWeb.TestEmbed do
  @moduledoc """
  Test embed templates for development and testing purposes.
  """

  use Phoenix.Component
  embed_templates("test_embed.*")
end
