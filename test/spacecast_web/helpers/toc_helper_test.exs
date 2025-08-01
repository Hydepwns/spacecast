defmodule SpacecastWeb.Helpers.TocHelperTest do
  use ExUnit.Case, async: true

  alias SpacecastWeb.Helpers.TocHelper

  describe "extract_headings/3" do
    test "extracts headings with IDs from HTML content" do
      html_content = """
      <h2 id="introduction">Introduction</h2>
      <p>Some text about the introduction</p>
      <h3 id="background">Background</h3>
      <p>Background information</p>
      <h2 id="features">Features</h2>
      <p>Description of features</p>
      """

      headings = TocHelper.extract_headings(html_content, 2, 4)

      assert length(headings) == 3
      assert Enum.at(headings, 0) == %{level: 2, id: "introduction", label: "Introduction"}
      assert Enum.at(headings, 1) == %{level: 3, id: "background", label: "Background"}
      assert Enum.at(headings, 2) == %{level: 2, id: "features", label: "Features"}
    end

    test "generates IDs for headings without IDs" do
      html_content = """
      <h2>Introduction</h2>
      <p>Some text</p>
      <h3>Background</h3>
      <p>More text</p>
      """

      headings = TocHelper.extract_headings(html_content, 2, 4)

      assert length(headings) == 2
      assert Enum.at(headings, 0).label == "Introduction"
      assert Enum.at(headings, 0).id == "introduction"
      assert Enum.at(headings, 1).label == "Background"
      assert Enum.at(headings, 1).id == "background"
    end

    test "handles headings with HTML content inside" do
      html_content = """
      <h2 id="section1"><span class="highlight">Important</span> Section</h2>
      <p>Content</p>
      """

      headings = TocHelper.extract_headings(html_content, 2, 4)

      assert length(headings) == 1
      assert Enum.at(headings, 0).label == "Important Section"
    end

    test "respects min_level and max_level parameters" do
      html_content = """
      <h1 id="title">Title</h1>
      <h2 id="section1">Section 1</h2>
      <h3 id="subsection">Subsection</h3>
      <h4 id="detail">Detail</h4>
      <h5 id="subdetail">Subdetail</h5>
      """

      # Only get h2 and h3
      headings = TocHelper.extract_headings(html_content, 2, 3)

      assert length(headings) == 2
      assert Enum.at(headings, 0).level == 2
      assert Enum.at(headings, 1).level == 3
    end
  end

  describe "build_toc_hierarchy/2" do
    test "builds hierarchy from flat list of headings" do
      headings = [
        %{level: 2, id: "intro", label: "Introduction"},
        %{level: 3, id: "background", label: "Background"},
        %{level: 3, id: "history", label: "History"},
        %{level: 2, id: "features", label: "Features"},
        %{level: 3, id: "feature1", label: "Feature 1"},
        %{level: 4, id: "detail1", label: "Detail 1"},
        %{level: 2, id: "conclusion", label: "Conclusion"}
      ]

      toc = TocHelper.build_toc_hierarchy(headings, "")

      # Top level should have 3 items
      assert length(toc) == 3

      # Check structure
      [intro, features, conclusion] = toc

      assert intro.label == "Introduction"
      assert length(intro.children) == 2
      assert Enum.at(intro.children, 0).label == "Background"
      assert Enum.at(intro.children, 1).label == "History"

      assert features.label == "Features"
      assert length(features.children) == 1
      assert Enum.at(features.children, 0).label == "Feature 1"
      assert length(Enum.at(features.children, 0).children) == 1
      assert Enum.at(features.children, 0).children |> Enum.at(0) |> Map.get(:label) == "Detail 1"

      assert conclusion.label == "Conclusion"
      assert Enum.empty?(conclusion.children)
    end

    test "applies id_prefix to all heading ids" do
      headings = [
        %{level: 2, id: "intro", label: "Introduction"},
        %{level: 3, id: "background", label: "Background"}
      ]

      toc = TocHelper.build_toc_hierarchy(headings, "section-")

      assert Enum.at(toc, 0).id == "section-intro"
      assert Enum.at(toc, 0).children |> Enum.at(0) |> Map.get(:id) == "section-background"
    end
  end

  describe "generate_toc/2" do
    test "generates complete TOC from HTML content" do
      html_content = """
      <h2 id="intro">Introduction</h2>
      <p>Some content</p>
      <h3 id="background">Background</h3>
      <p>More content</p>
      <h2 id="features">Features</h2>
      <h3 id="feature1">Feature 1</h3>
      <h4 id="detail1">Detail 1</h4>
      <h2 id="conclusion">Conclusion</h2>
      """

      toc = TocHelper.generate_toc(html_content)

      # Check top level structure
      assert length(toc) == 3
      assert Enum.map(toc, & &1.label) == ["Introduction", "Features", "Conclusion"]

      # Check first section
      first_section = Enum.at(toc, 0)
      assert first_section.children |> Enum.at(0) |> Map.get(:label) == "Background"

      # Check second section
      second_section = Enum.at(toc, 1)
      assert second_section.children |> Enum.at(0) |> Map.get(:label) == "Feature 1"
      feature1 = second_section.children |> Enum.at(0)
      assert feature1.children |> Enum.at(0) |> Map.get(:label) == "Detail 1"
    end
  end

  describe "flatten_toc/2" do
    test "flattens hierarchical TOC to a list" do
      toc = [
        %{
          id: "intro",
          label: "Introduction",
          level: 2,
          children: [
            %{id: "background", label: "Background", level: 3, children: []}
          ]
        },
        %{
          id: "features",
          label: "Features",
          level: 2,
          children: []
        }
      ]

      flat_toc = TocHelper.flatten_toc(toc)

      assert length(flat_toc) == 3
      assert Enum.at(flat_toc, 0).id == "intro"
      assert Enum.at(flat_toc, 0).label == "Introduction"
      assert Enum.at(flat_toc, 0).level == 2
      assert Enum.at(flat_toc, 1).id == "background"
      assert Enum.at(flat_toc, 1).label == "Background"
      assert Enum.at(flat_toc, 1).level == 3
      assert Enum.at(flat_toc, 2).id == "features"
      assert Enum.at(flat_toc, 2).label == "Features"
      assert Enum.at(flat_toc, 2).level == 2
    end
  end
end
