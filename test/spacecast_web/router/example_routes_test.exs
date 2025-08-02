defmodule SpacecastWeb.Router.ExampleRoutesTest do
  @moduledoc """
  This module tests the ExampleRoutes module.
  """
  use ExUnit.Case, async: true
  alias SpacecastWeb.Router.ExampleRoutes

  describe "ExampleRoutes module" do
    test "defines example routes" do
      # Test that the module can be used without errors
      assert Code.ensure_loaded(ExampleRoutes) == {:module, ExampleRoutes}

      # Test that the module has the expected macro
      assert macro_exported?(ExampleRoutes, :__using__, 1)
    end

    test "example routes module documentation" do
      # Test that the module has proper documentation
      {:docs_v1, _, _, _, %{"en" => moduledoc}, _, _} = Code.fetch_docs(ExampleRoutes)
      assert moduledoc != nil
      assert is_binary(moduledoc)
      assert String.length(moduledoc) > 0
    end

    test "example routes includes expected route types" do
      # This test verifies that the macro structure includes expected route patterns
      # Instead of expanding the macro, we'll check the source code directly
      source_file = "lib/spacecast_web/router/example_routes.ex"
      assert File.exists?(source_file)

      source_content = File.read!(source_file)

      # Check for expected route patterns in the source
      assert source_content =~ "live"
      assert source_content =~ "scope"
      assert source_content =~ "examples"
      assert source_content =~ "playground"
    end
  end
end
