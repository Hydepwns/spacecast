defmodule SpacecastCSSOrganizationTest do
  use ExUnit.Case, async: true

  describe "CSS Organization" do
    test "component CSS files exist" do
      css_files = [
        "assets/css/components/ascii-art-editor.css",
        "assets/css/components/system-monitor.css",
        "assets/css/components/code-flow-visualizer.css",
        "assets/css/components/network-topology.css",
        "assets/css/components/data-visualizer.css",
        "assets/css/components/examples-index.css",
        "assets/css/components/transformation-metrics.css",
        "assets/css/components/timeline_view.css",
        "assets/css/components/list_view.css",
        "assets/css/components/modal_components.css",
        "assets/css/components/_index.css"
      ]

      for file <- css_files do
        assert File.exists?(file), "CSS file #{file} should exist"
      end
    end

    test "CSS index file imports all components" do
      index_content = File.read!("assets/css/components/_index.css")

      # Check that the index file contains import statements
      assert index_content =~ "@import"

      # Check for specific component imports
      assert index_content =~ "ascii-art-editor.css"
      assert index_content =~ "system-monitor.css"
      assert index_content =~ "code-flow-visualizer.css"
      assert index_content =~ "network-topology.css"
      assert index_content =~ "data-visualizer.css"
      assert index_content =~ "examples-index.css"
      assert index_content =~ "transformation-metrics.css"
      assert index_content =~ "timeline_view.css"
      assert index_content =~ "list_view.css"
      assert index_content =~ "modal_components.css"
    end

    test "CSS files contain component-specific styles" do
      # Test that each CSS file contains appropriate component styles
      assert File.read!("assets/css/components/ascii-art-editor.css") =~ ".ascii-art-editor"
      assert File.read!("assets/css/components/system-monitor.css") =~ ".system-monitor"
      assert File.read!("assets/css/components/code-flow-visualizer.css") =~ ".code-flow-visualizer"
      assert File.read!("assets/css/components/network-topology.css") =~ ".network-topology"
      assert File.read!("assets/css/components/data-visualizer.css") =~ ".data-visualizer"
      assert File.read!("assets/css/components/examples-index.css") =~ ".examples-index"
      assert File.read!("assets/css/components/transformation-metrics.css") =~ ".transformation-metrics"
      assert File.read!("assets/css/components/timeline_view.css") =~ ".timeline-view"
      assert File.read!("assets/css/components/list_view.css") =~ ".list-view"
      assert File.read!("assets/css/components/modal_components.css") =~ ".modal"
    end

    test "CSS files use component-specific CSS custom properties" do
      index_content = File.read!("assets/css/components/_index.css")

      # Check that the index file defines component-specific CSS custom properties
      assert index_content =~ "--component-padding-sm"
      assert index_content =~ "--component-padding-md"
      assert index_content =~ "--component-padding-lg"
      assert index_content =~ "--component-padding-xl"
      assert index_content =~ "--component-radius-sm"
      assert index_content =~ "--component-radius-md"
      assert index_content =~ "--component-radius-lg"
      assert index_content =~ "--component-shadow-sm"
      assert index_content =~ "--component-shadow-md"
      assert index_content =~ "--component-shadow-lg"
      assert index_content =~ "--component-transition-fast"
      assert index_content =~ "--component-transition-normal"
      assert index_content =~ "--component-transition-slow"
    end

    test "CSS files include responsive design" do
      # Test that component CSS files include responsive design rules
      assert File.read!("assets/css/components/ascii-art-editor.css") =~ "@media"
      assert File.read!("assets/css/components/system-monitor.css") =~ "@media"
      assert File.read!("assets/css/components/code-flow-visualizer.css") =~ "@media"
      assert File.read!("assets/css/components/network-topology.css") =~ "@media"
      assert File.read!("assets/css/components/data-visualizer.css") =~ "@media"
      assert File.read!("assets/css/components/examples-index.css") =~ "@media"
      assert File.read!("assets/css/components/transformation-metrics.css") =~ "@media"
      assert File.read!("assets/css/components/timeline_view.css") =~ "@media"
      assert File.read!("assets/css/components/list_view.css") =~ "@media"
      assert File.read!("assets/css/components/modal_components.css") =~ "@media"
    end

    test "CSS files use consistent naming conventions" do
      # Test that component CSS files use BEM-like naming conventions
      assert File.read!("assets/css/components/transformation-metrics.css") =~ "__"
      assert File.read!("assets/css/components/timeline_view.css") =~ "timeline-"
      assert File.read!("assets/css/components/list_view.css") =~ "list-"
      assert File.read!("assets/css/components/modal_components.css") =~ "modal-"
    end

    test "CSS index file has proper organization structure" do
      index_content = File.read!("assets/css/components/_index.css")

      # Check for proper section organization
      assert index_content =~ "Base Component Styles"
      assert index_content =~ "Feature-Specific Components"
      assert index_content =~ "Business Logic Components"
      assert index_content =~ "Component-Specific Utilities"
    end

    test "CSS files are reasonably sized" do
      css_files = [
        "assets/css/components/ascii-art-editor.css",
        "assets/css/components/system-monitor.css",
        "assets/css/components/code-flow-visualizer.css",
        "assets/css/components/network-topology.css",
        "assets/css/components/data-visualizer.css",
        "assets/css/components/examples-index.css",
        "assets/css/components/transformation-metrics.css",
        "assets/css/components/timeline_view.css",
        "assets/css/components/list_view.css",
        "assets/css/components/modal_components.css"
      ]

      for file <- css_files do
        content = File.read!(file)
        line_count = length(String.split(content, "\n"))

        # Each component CSS file should be reasonably sized (not too large)
        assert line_count < 500, "CSS file #{file} is too large (#{line_count} lines)"

        # Each component CSS file should have some content
        assert line_count > 10, "CSS file #{file} is too small (#{line_count} lines)"
      end
    end
  end
end
