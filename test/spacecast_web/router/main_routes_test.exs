defmodule SpacecastWeb.Router.MainRoutesTest do
  use ExUnit.Case, async: true
  alias SpacecastWeb.Router.MainRoutes

  describe "MainRoutes module" do
    test "defines core page routes" do
      # Test that the module can be used without errors
      assert Code.ensure_loaded(MainRoutes) == {:module, MainRoutes}

      # Test that the module has the expected macro
      assert function_exported?(MainRoutes, :__using__, 1)
    end

    test "main routes module documentation" do
      # Test that the module has proper documentation
      module_info = MainRoutes.module_info(:attributes)
      assert Keyword.get(module_info, :moduledoc) != nil
    end

    test "main routes macro structure" do
      # Test that the macro returns a quoted expression
      quoted = MainRoutes.__using__([])
      assert is_list(quoted)
      assert length(quoted) > 0
    end

    test "main routes includes expected route types" do
      # This test verifies that the macro structure includes expected route patterns
      quoted = MainRoutes.__using__([])

      # Convert the quoted expression to a string to check for expected patterns
      quoted_string = Macro.to_string(quoted)

      # Check for expected route patterns
      assert quoted_string =~ "live"
      assert quoted_string =~ "HomeLive"
      assert quoted_string =~ "AboutLive"
    end
  end
end
