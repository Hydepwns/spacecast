defmodule SpacecastWeb.FixtureHelper do
  @moduledoc """
  Helper functions for generating test data.

  This module provides utilities for creating consistent test fixtures
  for use in tests throughout the application.
  """

  @doc """
  Generates a sample MonoGrid structure with the specified number of columns and rows.

  ## Parameters

  - `columns` - Number of columns (default: 3)
  - `rows` - Number of rows (default: 2)
  - `opts` - Additional options for customization

  ## Examples

      iex> generate_mono_grid()
      %{columns: 3, rows: 2, cells: [...]}
  """
  def generate_mono_grid(columns \\ 3, rows \\ 2, opts \\ []) do
    custom_class = Keyword.get(opts, :custom_class, "")

    cells =
      for row <- 1..rows, col <- 1..columns do
        %{
          row: row,
          col: col,
          content: "Cell #{row},#{col}",
          class: if(rem(row + col, 2) == 0, do: "even", else: "odd")
        }
      end

    %{
      columns: columns,
      rows: rows,
      cells: cells,
      class: custom_class
    }
  end

  @doc """
  Generates a sample Terminal structure with commands and output.

  ## Parameters

  - `command_count` - Number of commands to generate (default: 3)
  - `opts` - Additional options for customization

  ## Examples

      iex> generate_terminal(2)
      %{commands: [...], prompt: "user@host:~$", theme: "dark"}
  """
  def generate_terminal(command_count \\ 3, opts \\ []) do
    prompt = Keyword.get(opts, :prompt, "user@host:~$ ")
    theme = Keyword.get(opts, :theme, "dark")

    commands =
      for i <- 1..command_count do
        command = "command#{i}"

        output = """
        line 1 of output for #{command}
        line 2 of output for #{command}
        """

        %{
          command: command,
          output: output,
          id: "cmd-#{i}"
        }
      end

    %{
      commands: commands,
      prompt: prompt,
      theme: theme
    }
  end

  @doc """
  Generates a sample ThemePreview structure.

  ## Parameters

  - `themes` - List of themes to generate previews for (default: ["light", "dark", "dim"])
  - `opts` - Additional options for customization

  ## Examples

      iex> generate_theme_previews()
      %{themes: [%{theme: "light", title: "Light Theme"}, ...]}
  """
  def generate_theme_previews(themes \\ ["light", "dark", "dim"], opts \\ []) do
    title_prefix = Keyword.get(opts, :title_prefix, "")

    theme_previews =
      Enum.map(themes, fn theme ->
        %{
          theme: theme,
          title: "#{title_prefix}#{String.capitalize(theme)} Theme"
        }
      end)

    %{
      themes: theme_previews
    }
  end

  @doc """
  Generates sample diagram data for DiagramEditor.

  ## Parameters

  - `node_count` - Number of nodes to generate (default: 3)
  - `opts` - Additional options for customization

  ## Examples

      iex> generate_diagram_data()
      %{nodes: [...], edges: [...], settings: %{...}}
  """
  def generate_diagram_data(node_count \\ 3, opts \\ []) do
    theme = Keyword.get(opts, :theme, "light")
    editable = Keyword.get(opts, :editable, true)

    nodes =
      for i <- 1..node_count do
        %{
          id: "node-#{i}",
          label: "Node #{i}",
          position: %{
            x: 50 + i * 100,
            y: 50 + rem(i, 2) * 80
          }
        }
      end

    edges =
      for i <- 1..(node_count - 1) do
        %{
          id: "edge-#{i}",
          source: "node-#{i}",
          target: "node-#{i + 1}",
          label: "connects to"
        }
      end

    %{
      nodes: nodes,
      edges: edges,
      settings: %{
        theme: theme,
        editable: editable,
        grid_size: 10,
        snap_to_grid: true
      }
    }
  end

  @doc """
  Generates sample user data.

  ## Parameters

  - `opts` - Options for customizing the user data

  ## Examples

      iex> generate_user()
      %{name: "Test User", email: "user@example.com", preferences: %{...}}
  """
  def generate_user(opts \\ []) do
    id = Keyword.get(opts, :id, 1)
    theme_preference = Keyword.get(opts, :theme_preference, "light")

    %{
      id: id,
      name: "Test User #{id}",
      email: "user#{id}@example.com",
      preferences: %{
        theme: theme_preference,
        font_size: "medium",
        animations_enabled: true
      }
    }
  end

  @doc """
  Generates sample page content data.

  ## Parameters

  - `section_count` - Number of sections to generate (default: 3)
  - `opts` - Additional options for customization

  ## Examples

      iex> generate_page_content()
      %{title: "Sample Page", sections: [...]}
  """
  def generate_page_content(section_count \\ 3, opts \\ []) do
    title = Keyword.get(opts, :title, "Sample Page")

    sections =
      for i <- 1..section_count do
        %{
          id: "section-#{i}",
          title: "Section #{i}",
          content: """
          This is the content for section #{i}.
          It can span multiple lines and contain various elements.
          """
        }
      end

    %{
      title: title,
      sections: sections
    }
  end
end
