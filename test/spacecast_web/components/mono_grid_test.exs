defmodule SpacecastWeb.Components.MonoGridTest do
  use SpacecastWeb.ConnCase, async: false
  import Phoenix.LiveViewTest
  alias SpacecastWeb.Components.MonoGrid

  describe "mono_grid/1" do
    test "renders a basic grid" do
      assigns = %{id: "test-grid", rows: 2, cols: 20}

      html =
        render_component(
          fn assigns ->
            ~H"""
            <MonoGrid.mono_grid id={@id} cols={@cols}>
              Test content
            </MonoGrid.mono_grid>
            """
          end,
          assigns
        )

      assert html =~ "test-grid"
      assert html =~ "mono-grid"
      assert html =~ "Test content"
    end

    test "renders a grid with borders" do
      assigns = %{id: "test-grid", rows: 2, cols: 20}

      html =
        render_component(
          fn assigns ->
            ~H"""
            <MonoGrid.mono_grid id={@id} cols={@cols} class="mono-grid--bordered">
              Test content
            </MonoGrid.mono_grid>
            """
          end,
          assigns
        )

      assert html =~ "test-grid"
      assert html =~ "mono-grid"
      assert html =~ "mono-grid--bordered"
    end

    test "renders a grid with debug mode" do
      assigns = %{id: "test-grid", rows: 2, cols: 20}

      html =
        render_component(
          fn assigns ->
            ~H"""
            <MonoGrid.mono_grid id={@id} cols={@cols} debug={true}>
              Test content
            </MonoGrid.mono_grid>
            """
          end,
          assigns
        )

      assert html =~ "test-grid"
      assert html =~ "mono-grid"
      assert html =~ "mono-grid--debug"
    end
  end

  describe "mono_grid_cell/1" do
    test "renders a cell with correct positioning" do
      assigns = %{row: 1, col: 1, colspan: 10, rowspan: 1}

      html =
        render_component(
          fn assigns ->
            ~H"""
            <MonoGrid.mono_grid_cell cols={@colspan} rows={@rowspan}>
              Test content
            </MonoGrid.mono_grid_cell>
            """
          end,
          assigns
        )

      assert html =~ "mono-grid-cell"
      assert html =~ "style="
    end

    test "renders a cell with content" do
      assigns = %{row: 1, col: 1, colspan: 10, rowspan: 1}

      html =
        render_component(
          fn assigns ->
            ~H"""
            <MonoGrid.mono_grid_cell cols={@colspan} rows={@rowspan}>
              Cell content
            </MonoGrid.mono_grid_cell>
            """
          end,
          assigns
        )

      assert html =~ "mono-grid-cell"
      assert html =~ "Cell content"
    end

    test "renders a cell with alignment" do
      assigns = %{row: 1, col: 1, colspan: 10, rowspan: 1, align: :center}

      html =
        render_component(
          fn assigns ->
            ~H"""
            <MonoGrid.mono_grid_cell cols={@colspan} rows={@rowspan} align={@align}>
              Cell content
            </MonoGrid.mono_grid_cell>
            """
          end,
          assigns
        )

      assert html =~ "mono-grid-cell"
      assert html =~ "mono-grid-cell--center"
    end
  end

  describe "grid with cells" do
    test "renders a complete grid with cells" do
      assigns = %{id: "test-grid", rows: 2, cols: 20}

      html =
        render_component(
          fn assigns ->
            ~H"""
            <MonoGrid.mono_grid id={@id} cols={@cols}>
              <MonoGrid.mono_grid_cell cols={20} rows={1}>
                Header
              </MonoGrid.mono_grid_cell>
              <MonoGrid.mono_grid_cell cols={10} rows={1}>
                Left
              </MonoGrid.mono_grid_cell>
              <MonoGrid.mono_grid_cell cols={10} rows={1}>
                Right
              </MonoGrid.mono_grid_cell>
            </MonoGrid.mono_grid>
            """
          end,
          assigns
        )

      assert html =~ "test-grid"
      assert html =~ "mono-grid"
      assert html =~ "Header"
      assert html =~ "Left"
      assert html =~ "Right"
    end
  end
end
