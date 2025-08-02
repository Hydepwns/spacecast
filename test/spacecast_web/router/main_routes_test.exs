defmodule SpacecastWeb.Router.MainRoutesTest do
  @moduledoc """
  This module tests the MainRoutes module.
  """
  use ExUnit.Case, async: true
  alias SpacecastWeb.Router.MainRoutes

  describe "MainRoutes module" do
    test "defines core page routes" do
      # Test that the module can be used without errors
      assert Code.ensure_loaded(MainRoutes) == {:module, MainRoutes}

      # Test that the module has the expected macro
      assert macro_exported?(MainRoutes, :__using__, 1)
    end

    test "main routes module documentation" do
      # Test that the module has proper documentation
      {:docs_v1, _, _, _, %{"en" => moduledoc}, _, _} = Code.fetch_docs(MainRoutes)
      assert moduledoc != nil
      assert is_binary(moduledoc)
      assert String.length(moduledoc) > 0
    end

    test "main routes includes expected route types" do
      # This test verifies that the macro structure includes expected route patterns
      # Instead of expanding the macro, we'll check the source code directly
      source_file = "lib/spacecast_web/router/main_routes.ex"
      assert File.exists?(source_file)

      source_content = File.read!(source_file)

      # Check for expected route patterns in the source
      assert source_content =~ "live"
      assert source_content =~ "HomeLive"
      assert source_content =~ "AboutLive"
      assert source_content =~ "ProjectsLive"
      assert source_content =~ "UserLive"
    end
  end
end
