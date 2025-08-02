defmodule SpacecastWeb.Router.ExampleRoutesTest do
  use ExUnit.Case, async: true
  alias SpacecastWeb.Router.ExampleRoutes

  describe "ExampleRoutes module" do
    test "defines example routes" do
      # Test that the module can be used without errors
      assert Code.ensure_loaded(ExampleRoutes) == {:module, ExampleRoutes}

      # Test that the module has the expected macro
      assert function_exported?(ExampleRoutes, :__using__, 1)
    end

    test "example routes module documentation" do
      # Test that the module has proper documentation
      module_info = ExampleRoutes.module_info(:attributes)
      assert Keyword.get(module_info, :moduledoc) != nil
    end

    test "example routes macro structure" do
      # Test that the macro returns a quoted expression
      quoted = ExampleRoutes.__using__([])
      assert is_list(quoted)
      assert length(quoted) > 0
    end

    test "example routes includes expected route types" do
      # This test verifies that the macro structure includes expected route patterns
      quoted = ExampleRoutes.__using__([])

      # Convert the quoted expression to a string to check for expected patterns
      quoted_string = Macro.to_string(quoted)

      # Check for expected route patterns
      assert quoted_string =~ "live"
      assert quoted_string =~ "scope"
      assert quoted_string =~ "examples"
    end
  end
end
