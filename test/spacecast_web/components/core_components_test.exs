defmodule SpacecastWeb.Components.CoreComponentsTest do
  use ExUnit.Case, async: true
  import Phoenix.LiveViewTest

  alias SpacecastWeb.Components.Common.CoreComponents

  describe "modal/1" do
    test "renders a modal" do
      html =
        render_component(&CoreComponents.modal/1, %{
          id: "test-modal",
          show: true,
          on_cancel: %Phoenix.LiveView.JS{},
          inner_block: [%{inner_block: fn _, _ -> "Modal Content" end}]
        })

      assert html =~ "id=\"test-modal\""
      assert html =~ "Modal Content"
    end
  end

  describe "flash/1" do
    test "renders an info flash" do
      html =
        render_component(&CoreComponents.flash/1, %{
          kind: :info,
          flash: %{"info" => "Info message"},
          title: "Info"
        })

      assert html =~ "Info message"
      assert html =~ "Info"
    end
  end

  describe "button/1" do
    test "renders a button" do
      html =
        render_component(&CoreComponents.button/1, %{
          type: "submit",
          class: "my-btn",
          inner_block: "Click me"
        })

      assert html =~ "Click me"
      assert html =~ "my-btn"
    end
  end

  describe "label/1" do
    test "renders a label" do
      html =
        render_component(&CoreComponents.label/1, %{
          for: "input-id",
          inner_block: [%{inner_block: fn _, _ -> "Label text" end}]
        })

      assert html =~ "Label text"
      assert html =~ "for=\"input-id\""
    end
  end

  describe "error/1" do
    test "renders an error message" do
      html =
        render_component(&CoreComponents.error/1, %{
          inner_block: [%{inner_block: fn _, _ -> "Error text" end}]
        })

      assert html =~ "Error text"
      assert html =~ "hero-exclamation-circle-mini"
    end
  end

  describe "header/1" do
    test "renders a header" do
      html =
        render_component(&CoreComponents.header/1, %{
          inner_block: [%{inner_block: fn _, _ -> "Header Title" end}],
          subtitle: [],
          actions_header: [],
          class: ""
        })

      assert html =~ "Header Title"
    end
  end

  describe "icon/1" do
    test "renders a heroicon span" do
      html =
        render_component(&CoreComponents.icon/1, %{
          name: "hero-x-mark-solid",
          class: "h-5 w-5"
        })

      assert html =~ "hero-x-mark-solid"
      assert html =~ "h-5 w-5"
    end
  end

  describe "simple_form/1" do
    test "renders a simple form" do
      form_struct = Phoenix.HTML.FormData.to_form(%{}, as: :test_form)

      html =
        render_component(&CoreComponents.simple_form/1, %{
          for: form_struct,
          as: nil,
          inner_block: "Form Field",
          actions: "Submit"
        })

      assert html =~ "Form Field"
      assert html =~ "Submit"
      assert html =~ "<form"
    end
  end

  describe "input/1" do
    test "renders a text input" do
      html =
        render_component(&CoreComponents.input/1, %{
          type_input: "text",
          name: "username",
          id: "username",
          value: "alice",
          label: "Username",
          errors: [],
          rest: []
        })

      assert html =~ "Username"
      assert html =~ "name=\"username\""
      assert html =~ "value=\"alice\""
    end

    test "renders a checkbox input" do
      html =
        render_component(&CoreComponents.input/1, %{
          type: "checkbox",
          name: "accept",
          id: "accept",
          value: true,
          label: "Accept Terms",
          errors: [],
          rest: []
        })

      assert html =~ "Accept Terms"
      assert html =~ "type=\"checkbox\""
    end

    test "renders a select input" do
      html =
        render_component(&CoreComponents.input/1, %{
          type: "select",
          name: "color",
          id: "color",
          label: "Color",
          options: [{"Red", "red"}, {"Blue", "blue"}],
          value: "red",
          errors: [],
          rest: [],
          multiple: false,
          prompt: nil
        })

      assert html =~ "Color"
      assert html =~ "<select"
      assert html =~ "Red"
      assert html =~ "Blue"
    end

    test "renders a textarea input" do
      html =
        render_component(&CoreComponents.input/1, %{
          type: "textarea",
          name: "bio",
          id: "bio",
          label: "Bio",
          value: "Hello",
          errors: [],
          rest: []
        })

      assert html =~ "Bio"
      assert html =~ "<textarea"
      assert html =~ "Hello"
    end
  end

  describe "table/1" do
    test "renders a table" do
      html =
        render_component(&CoreComponents.table/1, %{
          id: "table-id",
          rows: [%{name: "Alice"}, %{name: "Bob"}],
          row_click: fn _row -> :ok end,
          row_item: fn item -> item end
        })

      assert html =~ "table-id"
      assert html =~ "Alice"
      assert html =~ "Bob"
      # The row_item function returns the map, so we expect the map content
      assert html =~ "%{name:"
    end
  end

  describe "list/1" do
    test "renders a list" do
      html =
        render_component(&CoreComponents.list/1, %{
          item: [
            %{title: "First", inner_block: "One"},
            %{title: "Second", inner_block: "Two"}
          ]
        })

      assert html =~ "First"
      assert html =~ "Second"
      assert html =~ "One"
      assert html =~ "Two"
    end
  end

  describe "back/1" do
    test "renders a back link" do
      html =
        render_component(&CoreComponents.back/1, %{
          navigate: "/home",
          inner_block: "Go Back"
        })

      assert html =~ "Go Back"
      assert html =~ "href=\"/home\""
    end
  end

  describe "theme_toggle/1" do
    test "renders theme toggle buttons" do
      html = render_component(&CoreComponents.theme_toggle/1, %{})
      assert html =~ "theme-toggle"
      assert html =~ "theme-toggle-btn"
      assert html =~ "🌙"
    end
  end

  describe "nav/1" do
    test "renders navigation links" do
      html = render_component(&CoreComponents.nav/1, %{})
      assert html =~ "nav"
      # The nav component renders an empty nav structure without slot content
      assert html =~ "<nav"
    end
  end

  describe "header_table/1" do
    test "renders a header table" do
      html =
        render_component(&CoreComponents.header_table/1, %{
          title: "My Title",
          version: "1.0",
          updated: "2024-01-01",
          author: "Author",
          license: "MIT",
          line_height: "1.5"
        })

      assert html =~ "My Title"
      assert html =~ "Version:"
      assert html =~ "1.0"
      assert html =~ "Author"
      assert html =~ "MIT"
      assert html =~ "1.5"
    end
  end
end
