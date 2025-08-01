defmodule Spacecast.Utils.ValidationErrorReporter do
  @moduledoc """
  Hierarchical error reporting for resource validations.

  This module provides functionality for collecting, organizing, and presenting
  validation errors in a hierarchical structure, making it easier to navigate
  and understand complex error states in nested resources.

  ## Features

  - **Hierarchical Error Structure**: Organize errors by resource and relationship
  - **Path-based Error Navigation**: Navigate errors using path expressions
  - **Error Visualization**: Format errors for display to users

  ## Examples

  ```elixir
  # Collect errors from validation
  {:error, errors} = ContextValidation.validate_deep(user)
  error_report = ValidationErrorReporter.create_error_report(errors)

  # Get errors for a specific resource path
  team_errors = ValidationErrorReporter.get_errors_at_path(error_report, "team")

  # Format errors for display
  html = ValidationErrorReporter.format_errors_as_html(error_report)
  ```
  """

  @doc """
  Creates a hierarchical error report from validation errors.

  This function takes the raw validation errors from `ContextValidation.validate_deep/2`
  and organizes them into a hierarchical structure that makes it easier to navigate
  and understand complex error states.

  ## Examples

  ```elixir
  # Create an error report
  error_report = ValidationErrorReporter.create_error_report(errors)
  ```

  ## Returns

  - Map representing the hierarchical error structure
  """
  @spec create_error_report(map()) :: map()
  def create_error_report(errors) do
    # Build the error tree structure
    Enum.reduce(errors, %{errors: [], children: %{}}, fn {resource_id, resource_errors}, acc ->
      # Extract resource module and ID
      {module, id} = extract_resource_id(resource_id)

      # Add errors to the tree
      add_errors_to_tree(acc, module, id, resource_errors)
    end)
  end

  @doc """
  Gets errors at a specific path in the error report.

  This function navigates the hierarchical error structure using a path expression
  and returns the errors at that location.

  ## Path Format

  The path is a dot-separated string that identifies a location in the error hierarchy.
  For example, "team.members.0" would get errors for the first member of the team.

  ## Examples

  ```elixir
  # Get errors for the team
  team_errors = ValidationErrorReporter.get_errors_at_path(error_report, "team")

  # Get errors for the first member of the team
  member_errors = ValidationErrorReporter.get_errors_at_path(error_report, "team.members.0")
  ```

  ## Returns

  - List of errors at the specified path
  - Empty list if no errors exist at the path or the path is invalid
  """
  @spec get_errors_at_path(map(), String.t()) :: list()
  def get_errors_at_path(error_report, path) do
    # Split the path into segments
    path_segments = String.split(path, ".")

    # Navigate to the target node
    navigate_to_path(error_report, path_segments)
    |> case do
      nil -> []
      node -> Map.get(node, :errors, [])
    end
  end

  @doc """
  Formats errors as HTML for display.

  This function takes an error report and produces HTML markup that visually
  represents the hierarchical error structure.

  ## Options

  - `:include_resource_ids` - Whether to include resource IDs in the output (default: false)
  - `:max_depth` - Maximum depth to include in the output (default: nil, meaning no limit)
  - `:style` - Style of HTML output: `:tree`, `:list`, or `:table` (default: `:tree`)

  ## Examples

  ```elixir
  # Format errors as HTML
  html = ValidationErrorReporter.format_errors_as_html(error_report)

  # Format with specific options
  html = ValidationErrorReporter.format_errors_as_html(error_report,
    include_resource_ids: true,
    max_depth: 2,
    style: :table
  )
  ```

  ## Returns

  - String containing HTML markup for the error report
  """
  @spec format_errors_as_html(map(), keyword()) :: String.t()
  def format_errors_as_html(error_report, opts \\ []) do
    include_ids = Keyword.get(opts, :include_resource_ids, false)
    max_depth = Keyword.get(opts, :max_depth, nil)
    style = Keyword.get(opts, :style, :tree)

    # Format based on style
    case style do
      :tree -> format_errors_as_tree_html(error_report, include_ids, max_depth)
      :list -> format_errors_as_list_html(error_report, include_ids, max_depth)
      :table -> format_errors_as_table_html(error_report, include_ids, max_depth)
      # Default to tree
      _ -> format_errors_as_tree_html(error_report, include_ids, max_depth)
    end
  end

  @doc """
  Formats errors as JSON for API responses.

  This function takes an error report and produces a JSON-compatible structure
  that represents the hierarchical error structure.

  ## Options

  - `:include_resource_ids` - Whether to include resource IDs in the output (default: false)
  - `:flatten` - Whether to flatten the error structure (default: false)
  - `:max_depth` - Maximum depth to include in the output (default: nil, meaning no limit)

  ## Examples

  ```elixir
  # Format errors as JSON
  json = ValidationErrorReporter.format_errors_as_json(error_report)

  # Format with specific options
  json = ValidationErrorReporter.format_errors_as_json(error_report,
    include_resource_ids: true,
    flatten: true,
    max_depth: 2
  )
  ```

  ## Returns

  - Map that can be encoded to JSON
  """
  @spec format_errors_as_json(map(), keyword()) :: map()
  def format_errors_as_json(error_report, opts \\ []) do
    include_ids = Keyword.get(opts, :include_resource_ids, false)
    flatten = Keyword.get(opts, :flatten, false)
    max_depth = Keyword.get(opts, :max_depth, nil)

    if flatten do
      flatten_error_tree(error_report, "", include_ids, max_depth)
    else
      format_error_tree_as_json(error_report, include_ids, max_depth)
    end
  end

  @doc """
  Counts the total number of errors in the report.

  This function recursively counts all errors in the hierarchical error structure.

  ## Examples

  ```elixir
  # Count total errors
  total_errors = ValidationErrorReporter.count_errors(error_report)
  ```

  ## Returns

  - Integer representing the total number of errors
  """
  @spec count_errors(map()) :: integer()
  def count_errors(error_report) do
    # Count errors at this level
    error_count = length(Map.get(error_report, :errors, []))

    # Add errors from children
    children = Map.get(error_report, :children, %{})

    child_error_count =
      Enum.reduce(children, 0, fn {_key, child}, acc ->
        acc + count_errors(child)
      end)

    error_count + child_error_count
  end

  @doc """
  Summarizes errors by type and count.

  This function provides a summary of the errors in the report, organized by
  error type and with counts of how many times each type occurs.

  ## Examples

  ```elixir
  # Summarize errors
  summary = ValidationErrorReporter.summarize_errors(error_report)
  ```

  ## Returns

  - Map with error types as keys and counts as values
  """
  @spec summarize_errors(map()) :: map()
  def summarize_errors(error_report) do
    # Start with empty summary
    empty_summary = %{}

    # Process errors at this level
    summary = process_errors_for_summary(Map.get(error_report, :errors, []), empty_summary)

    # Process errors from children
    children = Map.get(error_report, :children, %{})

    Enum.reduce(children, summary, fn {_key, child}, acc ->
      # Get summary for this child
      child_summary = summarize_errors(child)

      # Merge with accumulated summary
      merge_summaries(acc, child_summary)
    end)
  end

  # Extract module and ID from a resource identifier
  defp extract_resource_id(resource_id) do
    case resource_id do
      {module, id} -> {module, id}
      other -> {nil, other}
    end
  end

  # Add errors to the hierarchical tree structure
  defp add_errors_to_tree(tree, module, id, errors) do
    # Process errors to extract path information
    {direct_errors, path_errors} = split_errors_by_path(errors)

    # Add direct errors to the tree
    tree = add_direct_errors(tree, module, id, direct_errors)

    # Process path errors
    Enum.reduce(path_errors, tree, fn {path, error}, acc ->
      add_path_error(acc, path, error)
    end)
  end

  # Split errors into direct errors and path errors
  defp split_errors_by_path(errors) do
    Enum.reduce(errors, {[], []}, fn error, {direct, path} ->
      case error do
        {rule, message} ->
          # Simple rule/message error
          {[{rule, message} | direct], path}

        %{relationship_path: path} = error_map ->
          # Error with a relationship path
          path_error = {path, Map.delete(error_map, :relationship_path)}
          {direct, [path_error | path]}

        other ->
          # Other error format, treat as direct
          {[other | direct], path}
      end
    end)
  end

  # Add direct errors to the tree at the current level
  defp add_direct_errors(tree, module, id, direct_errors) do
    # Create error entries with module and ID information
    formatted_errors =
      Enum.map(direct_errors, fn error ->
        format_error(error, module, id)
      end)

    # Add errors to the current node
    existing_errors = Map.get(tree, :errors, [])
    %{tree | errors: existing_errors ++ formatted_errors}
  end

  # Format an error with module and ID information
  defp format_error(error, module, id) do
    case error do
      {rule, message} ->
        %{
          rule: rule,
          message: message,
          module: module,
          id: id
        }

      %{} = error_map ->
        Map.merge(error_map, %{module: module, id: id})

      other ->
        %{
          error: other,
          module: module,
          id: id
        }
    end
  end

  # Add an error with a relationship path
  defp add_path_error(tree, path, error) do
    case path do
      [first | rest] ->
        # Get or create the child node for this relationship
        children = Map.get(tree, :children, %{})
        child = Map.get(children, first, %{errors: [], children: %{}})

        # Process the error
        updated_child =
          if Enum.empty?(rest) do
            # This is the target node, add the error here
            existing_errors = Map.get(child, :errors, [])
            %{child | errors: existing_errors ++ [error]}
          else
            # Continue down the path
            add_path_error(child, rest, error)
          end

        # Update the tree
        %{tree | children: Map.put(children, first, updated_child)}

      [] ->
        # If path is empty, just add the error at the root
        existing_errors = Map.get(tree, :errors, [])
        %{tree | errors: existing_errors ++ [error]}
    end
  end

  # Navigate to a specific path in the error tree
  defp navigate_to_path(tree, []) do
    # We've reached the target node
    tree
  end

  defp navigate_to_path(tree, [segment | rest]) do
    # Get children at this level
    children = Map.get(tree, :children, %{})

    # Look for the child node
    case Map.get(children, segment) do
      nil ->
        # Try to interpret segment as an index
        case Integer.parse(segment) do
          {index, ""} ->
            # This is an array index, try to find an indexed child
            indexed_children =
              Enum.filter(children, fn {key, _} ->
                case Integer.parse(to_string(key)) do
                  {^index, ""} -> true
                  _ -> false
                end
              end)

            case indexed_children do
              [{_key, child} | _] -> navigate_to_path(child, rest)
              [] -> nil
            end

          _ ->
            nil
        end

      child ->
        # Continue down the path
        navigate_to_path(child, rest)
    end
  end

  # Format error tree as HTML tree view
  defp format_errors_as_tree_html(error_report, include_ids, max_depth) do
    """
    <div class="validation-errors-tree">
      #{format_node_as_tree_html(error_report, "", include_ids, max_depth, 0)}
    </div>
    """
  end

  # Format a single node as HTML tree view
  defp format_node_as_tree_html(node, prefix, include_ids, max_depth, current_depth) do
    # Check depth limit
    if not is_nil(max_depth) and current_depth > max_depth do
      "<div class=\"truncated-depth\">...</div>"
    else
      # Format errors at this level
      errors_html = format_errors_html(Map.get(node, :errors, []), include_ids)

      # Format children
      children = Map.get(node, :children, %{})

      children_html =
        if Enum.empty?(children) do
          ""
        else
          """
          <ul class="error-children">
            #{Enum.map(children, fn {key, child} -> """
            <li class="error-child">
              <div class="error-path">#{prefix}#{key}</div>
              #{format_node_as_tree_html(child, "#{prefix}#{key}.", include_ids, max_depth, current_depth + 1)}
            </li>
            """ end) |> Enum.join("")}
          </ul>
          """
        end

      # Combine errors and children
      """
      <div class="error-node">
        #{errors_html}
        #{children_html}
      </div>
      """
    end
  end

  # Format error tree as HTML list view
  defp format_errors_as_list_html(error_report, include_ids, max_depth) do
    """
    <div class="validation-errors-list">
      <ul class="error-list">
        #{flatten_error_tree_to_html_list(error_report, "", include_ids, max_depth, 0)}
      </ul>
    </div>
    """
  end

  # Flatten error tree to HTML list items
  defp flatten_error_tree_to_html_list(node, path, include_ids, max_depth, current_depth) do
    # Check depth limit
    if not is_nil(max_depth) and current_depth > max_depth do
      "<li class=\"truncated-depth\">#{path}...</li>"
    else
      # Get errors at this level
      errors = Map.get(node, :errors, [])

      # Format errors at this level
      errors_html =
        if Enum.empty?(errors) do
          ""
        else
          Enum.map(errors, fn error ->
            """
            <li class="error-item">
              <span class="error-path">#{path}</span>
              <span class="error-message">#{format_error_message(error, include_ids)}</span>
            </li>
            """
          end)
          |> Enum.join("")
        end

      # Format children
      children = Map.get(node, :children, %{})

      children_html =
        Enum.map(children, fn {key, child} ->
          child_path = if path == "", do: key, else: "#{path}.#{key}"

          flatten_error_tree_to_html_list(
            child,
            child_path,
            include_ids,
            max_depth,
            current_depth + 1
          )
        end)
        |> Enum.join("")

      # Combine errors and children
      errors_html <> children_html
    end
  end

  # Format error tree as HTML table view
  defp format_errors_as_table_html(error_report, include_ids, max_depth) do
    """
    <table class="validation-errors-table">
      <thead>
        <tr>
          <th>Path</th>
          <th>Error</th>
          #{if include_ids, do: "<th>Resource</th><th>ID</th>", else: ""}
        </tr>
      </thead>
      <tbody>
        #{flatten_error_tree_to_html_table_rows(error_report, "", include_ids, max_depth, 0)}
      </tbody>
    </table>
    """
  end

  # Flatten error tree to HTML table rows
  defp flatten_error_tree_to_html_table_rows(node, path, include_ids, max_depth, current_depth) do
    # Check depth limit
    if not is_nil(max_depth) and current_depth > max_depth do
      """
      <tr class="truncated-depth">
        <td>#{path}</td>
        <td>...</td>
        #{if include_ids, do: "<td></td><td></td>", else: ""}
      </tr>
      """
    else
      # Get errors at this level
      errors = Map.get(node, :errors, [])

      # Format errors at this level
      errors_html =
        if Enum.empty?(errors) do
          ""
        else
          Enum.map(errors, fn error ->
            """
            <tr class="error-row">
              <td class="error-path">#{path}</td>
              <td class="error-message">#{get_error_message(error)}</td>
              #{if include_ids, do: "<td>#{get_error_module(error)}</td><td>#{get_error_id(error)}</td>", else: ""}
            </tr>
            """
          end)
          |> Enum.join("")
        end

      # Format children
      children = Map.get(node, :children, %{})

      children_html =
        Enum.map(children, fn {key, child} ->
          child_path = if path == "", do: key, else: "#{path}.#{key}"

          flatten_error_tree_to_html_table_rows(
            child,
            child_path,
            include_ids,
            max_depth,
            current_depth + 1
          )
        end)
        |> Enum.join("")

      # Combine errors and children
      errors_html <> children_html
    end
  end

  # Format errors as HTML
  defp format_errors_html(errors, include_ids) do
    if Enum.empty?(errors) do
      ""
    else
      """
      <ul class="error-messages">
        #{Enum.map(errors, fn error -> """
        <li class="error-message">
          #{format_error_message(error, include_ids)}
        </li>
        """ end) |> Enum.join("")}
      </ul>
      """
    end
  end

  # Format an individual error message
  defp format_error_message(error, include_ids) do
    message = get_error_message(error)

    if include_ids do
      module = get_error_module(error)
      id = get_error_id(error)
      "#{message} (#{module} ##{id})"
    else
      message
    end
  end

  # Get error message from error structure
  defp get_error_message(error) do
    cond do
      is_map(error) && Map.has_key?(error, :message) ->
        error.message

      is_map(error) && Map.has_key?(error, :error) ->
        inspect(error.error)

      true ->
        inspect(error)
    end
  end

  # Get module from error structure
  defp get_error_module(error) do
    if is_map(error) && Map.has_key?(error, :module) do
      module = error.module

      if is_atom(module) do
        module |> Atom.to_string() |> String.replace("Elixir.", "")
      else
        inspect(module)
      end
    else
      "Unknown"
    end
  end

  # Get ID from error structure
  defp get_error_id(error) do
    if is_map(error) && Map.has_key?(error, :id) do
      error.id
    else
      "Unknown"
    end
  end

  # Format error tree as JSON
  defp format_error_tree_as_json(tree, include_ids, max_depth, current_depth \\ 0) do
    # Check depth limit
    if not is_nil(max_depth) and current_depth > max_depth do
      %{truncated: true}
    else
      # Format errors at this level
      errors =
        Map.get(tree, :errors, [])
        |> Enum.map(fn error ->
          if include_ids do
            %{
              message: get_error_message(error),
              module: get_error_module(error),
              id: get_error_id(error)
            }
          else
            %{message: get_error_message(error)}
          end
        end)

      # Format children
      children = Map.get(tree, :children, %{})

      formatted_children =
        Enum.map(children, fn {key, child} ->
          {key, format_error_tree_as_json(child, include_ids, max_depth, current_depth + 1)}
        end)
        |> Enum.into(%{})

      # Combine errors and children
      %{
        errors: errors,
        children: formatted_children
      }
    end
  end

  # Flatten error tree to path-based structure
  defp flatten_error_tree(tree, path_prefix, include_ids, max_depth, current_depth \\ 0) do
    # Check depth limit
    if not is_nil(max_depth) and current_depth > max_depth do
      %{"#{path_prefix}" => [%{truncated: true}]}
    else
      # Get errors at this level
      errors = Map.get(tree, :errors, [])

      # Format errors at this level
      current_errors =
        if Enum.empty?(errors) do
          %{}
        else
          formatted_errors =
            Enum.map(errors, fn error ->
              if include_ids do
                %{
                  message: get_error_message(error),
                  module: get_error_module(error),
                  id: get_error_id(error)
                }
              else
                %{message: get_error_message(error)}
              end
            end)

          %{"#{path_prefix}" => formatted_errors}
        end

      # Format children
      children = Map.get(tree, :children, %{})

      # Process each child
      child_errors =
        Enum.reduce(children, %{}, fn {key, child}, acc ->
          child_path = if path_prefix == "", do: key, else: "#{path_prefix}.#{key}"

          child_errors =
            flatten_error_tree(child, child_path, include_ids, max_depth, current_depth + 1)

          # Merge with accumulated errors
          Map.merge(acc, child_errors)
        end)

      # Merge current errors with child errors
      Map.merge(current_errors, child_errors)
    end
  end

  # Process errors for summary
  defp process_errors_for_summary(errors, summary) do
    Enum.reduce(errors, summary, fn error, acc ->
      # Extract error type
      error_type =
        cond do
          is_map(error) && Map.has_key?(error, :rule) ->
            error.rule

          is_map(error) && Map.has_key?(error, :error) ->
            :general

          true ->
            :unknown
        end

      # Update count for this error type
      Map.update(acc, error_type, 1, &(&1 + 1))
    end)
  end

  # Merge two error summaries
  defp merge_summaries(summary1, summary2) do
    Map.merge(summary1, summary2, fn _key, count1, count2 ->
      count1 + count2
    end)
  end
end
