defmodule SpacecastWeb.Helpers.TocHelper do
  @moduledoc """
  Helper module for generating Table of Contents (TOC) from HTML content.

  This module provides functionality to:
  1. Parse HTML content and extract heading elements (h2, h3, h4, etc.)
  2. Build a hierarchical TOC structure
  3. Generate TOC items with proper nesting and relationships

  Used to automatically generate TOC instead of manual definition in LiveViews.
  """

  @doc """
  Extracts headings from HTML content and generates a hierarchical table of contents.

  ## Parameters

  * `html_content` - String containing HTML content with heading elements
  * `opts` - Options for customizing the TOC generation:
    * `:min_level` - Minimum heading level to include (default: 2 for h2)
    * `:max_level` - Maximum heading level to include (default: 4 for h4)
    * `:id_prefix` - Prefix to add to heading IDs if needed (default: "")

  ## Returns

  A nested map structure representing the TOC with the following format:
  ```
  [
    %{
      id: "section-id",
      label: "Section Title",
      level: 2,
      children: [
        %{
          id: "subsection-id",
          label: "Subsection Title",
          level: 3,
          children: []
        }
      ]
    },
    ...
  ]
  ```

  ## Examples

  ```elixir
  html_content = "
    <h2 id='intro'>Introduction</h2>
    <p>Some content</p>
    <h3 id='background'>Background</h3>
    <p>More content</p>
    <h2 id='features'>Features</h2>
  "

  TocHelper.generate_toc(html_content)
  # Returns:
  # [
  #   %{id: "intro", label: "Introduction", level: 2, children: [
  #     %{id: "background", label: "Background", level: 3, children: []}
  #   ]},
  #   %{id: "features", label: "Features", level: 2, children: []}
  # ]
  ```
  """
  def generate_toc(html_content, opts \\ []) do
    min_level = Keyword.get(opts, :min_level, 2)
    max_level = Keyword.get(opts, :max_level, 4)
    id_prefix = Keyword.get(opts, :id_prefix, "")

    # Extract headings from the HTML content
    headings = extract_headings(html_content, min_level, max_level)

    # Build hierarchical TOC structure
    build_toc_hierarchy(headings, id_prefix)
  end

  @doc """
  Extracts heading elements from HTML content.

  Returns a list of maps with heading information: id, text, level.
  """
  def extract_headings(html_content, min_level, max_level) do
    # Create a regex pattern to match heading elements
    # This pattern:
    # 1. Matches <h2> to <h6> tags with optional attributes
    # 2. Captures the heading level, id attribute, and content text
    heading_pattern =
      ~r/<h([#{min_level}-#{max_level}])(?:\s+[^>]*?id=["\']([^"\']*)["\']|[^>]*?)>(.*?)<\/h\1>/si

    # Find all matches in the HTML content
    Regex.scan(heading_pattern, html_content, capture: :all_but_first)
    |> Enum.map(fn match ->
      case match do
        [level, id, content] when id != nil and id != "" ->
          %{
            level: String.to_integer(level),
            id: id,
            label: sanitize_heading_text(content)
          }

        [level, _id, content] ->
          generated_id = generate_id_from_text(content)

          %{
            level: String.to_integer(level),
            id: generated_id,
            label: sanitize_heading_text(content)
          }
      end
    end)
  end

  @doc """
  Builds a hierarchical TOC structure from a flat list of headings.

  Creates proper parent-child relationships based on heading levels.
  """
  def build_toc_hierarchy(headings, id_prefix) do
    if Enum.empty?(headings) do
      []
    else
      headings_with_prefix =
        Enum.map(headings, fn heading ->
          Map.update!(heading, :id, fn id -> id_prefix <> id end)
        end)

      min_level_present = Enum.min_by(headings_with_prefix, & &1.level).level
      initial_parent_level = min_level_present - 1

      {children, _remaining_headings} =
        do_build_hierarchy(headings_with_prefix, initial_parent_level)

      children
    end
  end

  defp do_build_hierarchy([], _parent_level), do: {[], []}

  defp do_build_hierarchy([heading | rest], parent_level) do
    current_level = heading.level

    if current_level == parent_level + 1 do
      {children, remaining} = collect_children(rest, current_level)
      {siblings, final_remaining} = do_build_hierarchy(remaining, parent_level)
      {[Map.put(heading, :children, children) | siblings], final_remaining}
    else
      {[], [heading | rest]}
    end
  end

  defp collect_children([], _parent_level), do: {[], []}

  defp collect_children([heading | rest], parent_level) do
    if heading.level > parent_level do
      {children, remaining} = do_build_hierarchy([heading | rest], parent_level)
      {children, remaining}
    else
      {[], [heading | rest]}
    end
  end

  @doc """
  Sanitizes heading text content by removing HTML tags and normalizing whitespace.
  """
  def sanitize_heading_text(content) do
    content
    |> String.replace(~r/<[^>]*>/, "")
    |> String.trim()
  end

  @doc """
  Generates an ID slug from heading text content.

  This is used when a heading doesn't already have an ID attribute.
  """
  def generate_id_from_text(content) do
    content
    |> sanitize_heading_text()
    |> String.downcase()
    |> String.replace(~r/[^a-z0-9\s-]/, "")
    |> String.replace(~r/\s+/, "-")
  end

  @doc """
  Converts the hierarchical TOC to a flat list format for simpler rendering.

  Useful when you don't need the full hierarchy but need level information.

  ## Example

  ```elixir
  toc = TocHelper.generate_toc(html_content)
  flat_toc = TocHelper.flatten_toc(toc)
  # Returns a list of %{id: "...", label: "...", level: n} items
  ```
  """
  def flatten_toc(toc, acc \\ []) do
    Enum.reduce(toc, acc, fn item, items ->
      current = Map.delete(item, :children)
      children_items = flatten_toc(Map.get(item, :children, []), [])
      items ++ [current] ++ children_items
    end)
  end
end
