defmodule SpacecastWeb.Components.UI.GeneralHelpersTest do
  use SpacecastWeb.ConnCase, async: true
  alias SpacecastWeb.Components.UI.GeneralHelpers

  describe "error handling functions" do
    test "error_count/1 with list of errors" do
      errors = [%{id: 1}, %{id: 2}, %{id: 3}]
      assert GeneralHelpers.error_count(errors) == 3
    end

    test "error_count/1 with non-list input" do
      assert GeneralHelpers.error_count(nil) == 0
      assert GeneralHelpers.error_count(%{id: 1}) == 0
    end

    test "error_icon/1 returns appropriate icon" do
      assert GeneralHelpers.error_icon(:critical) == "✗"
      assert GeneralHelpers.error_icon(:warning) == "!"
      assert GeneralHelpers.error_icon(:info) == "i"
    end

    test "get_error_id/1 extracts ID from different error formats" do
      assert GeneralHelpers.get_error_id(%{id: 123}) == 123
      assert GeneralHelpers.get_error_id(%{error_id: 456}) == 456
      assert GeneralHelpers.get_error_id(%{}) == nil
    end

    test "get_error_message/1 extracts message from different error formats" do
      assert GeneralHelpers.get_error_message(%{message: "Test error"}) == "Test error"

      assert GeneralHelpers.get_error_message(%{error: %{message: "Nested error"}}) ==
               "Nested error"

      assert GeneralHelpers.get_error_message("String error") == "String error"
      assert GeneralHelpers.get_error_message(%{}) == "Unknown error"
    end

    test "get_error_module/1 extracts module from different error formats" do
      assert GeneralHelpers.get_error_module(%{module: TestModule}) == TestModule
      assert GeneralHelpers.get_error_module(%{error_module: TestModule}) == TestModule
      assert GeneralHelpers.get_error_module(%{}) == nil
    end

    test "most_affected_view/1 finds view with most errors" do
      errors = [
        %{module: ModuleA, message: "Error 1"},
        %{module: ModuleA, message: "Error 2"},
        %{module: ModuleB, message: "Error 3"}
      ]

      assert GeneralHelpers.most_affected_view(errors) == ModuleA
    end

    test "most_common_error_type/1 finds most frequent error type" do
      errors = [
        %{type: :validation, message: "Error 1"},
        %{type: :validation, message: "Error 2"},
        %{type: :system, message: "Error 3"}
      ]

      assert GeneralHelpers.most_common_error_type(errors) == :validation
    end

    test "unique_error_types/1 returns unique error types" do
      errors = [
        %{type: :validation, message: "Error 1"},
        %{type: :validation, message: "Error 2"},
        %{type: :system, message: "Error 3"}
      ]

      assert GeneralHelpers.unique_error_types(errors) == [:validation, :system]
    end
  end

  describe "UI rendering functions" do
    test "render_errors/4 renders error component" do
      errors = [%{message: "Test error"}]

      _assigns = %{
        errors: errors,
        level: :critical,
        title: "Error Title",
        content: "Error Content"
      }

      html = GeneralHelpers.render_errors(errors, :critical, "Error Title", "Error Content")
      assert html =~ "Error Title"
      assert html =~ "Error Content"
      assert html =~ "Test error"
    end

    test "render_validation_status/3 renders validation status" do
      _assigns = %{valid: true, message: "Success message", type: "Validation"}
      html = GeneralHelpers.render_validation_status(true, "Success message", "Validation")
      assert html =~ "Success message"
      assert html =~ "Validation"
      assert html =~ "bg-green-50"

      _assigns = %{valid: false, message: "Error message", type: "Validation"}
      html = GeneralHelpers.render_validation_status(false, "Error message", "Validation")
      assert html =~ "Error message"
      assert html =~ "Validation"
      assert html =~ "bg-red-50"
    end
  end

  describe "data formatting functions" do
    test "display_field_value/2 formats different field types" do
      datetime = ~U[2024-03-20 12:00:00Z]
      assert GeneralHelpers.display_field_value(:datetime, datetime) =~ "2024-03-20"
      assert GeneralHelpers.display_field_value(:date, datetime) =~ "2024-03-20"
      assert GeneralHelpers.display_field_value(:time, datetime) =~ "12:00"
      assert GeneralHelpers.display_field_value(:percentage, 0.5) =~ "50%"
      assert GeneralHelpers.display_field_value(:size, 1024) =~ "1 KB"
      assert GeneralHelpers.display_field_value(:status, "active") =~ "Active"
      assert GeneralHelpers.display_field_value(:unknown, "value") =~ "value"
    end

    test "truncate_message/2 truncates long messages" do
      message = "This is a very long message that needs to be truncated"
      assert GeneralHelpers.truncate_message(message, 10) == "This is a..."
      assert GeneralHelpers.truncate_message(message, 100) == message
      assert GeneralHelpers.truncate_message(nil, 10) == nil
    end

    test "short_view_name/1 shortens view names" do
      assert GeneralHelpers.short_view_name("SpacecastWeb.UserView") == "User"

      assert GeneralHelpers.short_view_name("SpacecastWeb.Admin.DashboardView") ==
               "Dashboard"
    end

    test "status_icon/1 returns appropriate icon" do
      assert GeneralHelpers.status_icon("success") == "✓"
      assert GeneralHelpers.status_icon("error") == "✗"
      assert GeneralHelpers.status_icon("warning") == "!"
      assert GeneralHelpers.status_icon("info") == "i"
      assert GeneralHelpers.status_icon("unknown") == "•"
    end
  end

  describe "utility functions" do
    test "build_filter/3 creates filter map" do
      filter = GeneralHelpers.build_filter(:name, :equals, "test")
      assert filter == %{field: :name, operator: :equals, value: "test"}
    end

    test "ensure_resource_module/1 checks module existence" do
      assert {:ok, Kernel} == GeneralHelpers.ensure_resource_module(Kernel)
      assert {:error, _} = GeneralHelpers.ensure_resource_module(NonExistentModule)
    end

    test "get_transformation_name/1 extracts name from different formats" do
      assert GeneralHelpers.get_transformation_name(%{name: "Test"}) == "Test"
      assert GeneralHelpers.get_transformation_name(%{transformation_name: "Test"}) == "Test"
      assert GeneralHelpers.get_transformation_name(%{}) == "Unknown transformation"
    end

    test "relationship_name_from_def/1 extracts name from different formats" do
      assert GeneralHelpers.relationship_name_from_def(%{name: "Test"}) == "Test"
      assert GeneralHelpers.relationship_name_from_def(%{relationship_name: "Test"}) == "Test"
      assert GeneralHelpers.relationship_name_from_def(%{}) == "Unknown relationship"
    end

    test "unset_other_defaults/2 updates default status" do
      items = [
        %{id: 1, is_default: true},
        %{id: 2, is_default: true},
        %{id: 3, is_default: false}
      ]

      result = GeneralHelpers.unset_other_defaults(items, 2)
      assert Enum.find(result, &(&1.id == 2)).is_default == true
      assert Enum.all?(result -- [Enum.find(result, &(&1.id == 2))], &(&1.is_default == false))
    end
  end
end
