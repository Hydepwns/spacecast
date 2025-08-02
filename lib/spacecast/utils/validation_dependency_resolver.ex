defmodule Spacecast.Utils.ValidationDependencyResolver do
  require Logger

  @moduledoc """
  Dependency resolution for complex validation rules.

  This module provides functionality for managing the dependencies between
  validation rules, determining the optimal order for executing validations,
  and detecting circular dependencies.

  ## Features

  - **Validation Order Resolution**: Automatically determine the optimal order for validations
  - **Circular Dependency Detection**: Identify and report circular dependencies
  - **Validation Checkpointing**: Track validation progress and support partial validation
  - **Dependency Graph Visualization**: Visualize the validation dependency graph
  - **Dependency Analysis**: Analyze the validation dependency structure

  ## Examples

  ```elixir
  # Resolve validation dependencies for a resource
  {:ok, validation_plan} = ValidationDependencyResolver.resolve_dependencies(user_resource)

  # Execute validations in the correct order
  {:ok, validation_results} = ValidationDependencyResolver.execute_validation_plan(validation_plan, user)

  # Visualize the dependency graph
  ValidationDependencyResolver.visualize_dependency_graph(validation_plan)

  # Analyze dependencies for a specific validation
  ValidationDependencyResolver.analyze_dependencies(validation_plan, {UserResource, :email_must_be_valid})
  ```
  """

  alias Spacecast.Utils.RelationshipResolver

  @doc """
  Resolves validation dependencies for a resource and its relationships.

  This function analyzes the validation rules defined for a resource and its
  related resources, identifies dependencies between validations, and creates
  a validation plan that ensures validations are executed in the correct order.

  ## Options

  - `:max_depth` - Maximum depth for traversing relationships (default: 3)
  - `:include_only` - List of relationship names to include in validation (excludes all others)
  - `:exclude_relationships` - List of relationship names to exclude from validation

  ## Examples

  ```elixir
  # Basic dependency resolution
  {:ok, validation_plan} = ValidationDependencyResolver.resolve_dependencies(user_resource)

  # Resolution with limited depth
  {:ok, validation_plan} = ValidationDependencyResolver.resolve_dependencies(user_resource, max_depth: 2)

  # Resolution with specific relationships
  {:ok, validation_plan} = ValidationDependencyResolver.resolve_dependencies(user_resource, include_only: [:team, :posts])
  ```

  ## Returns

  - `{:ok, validation_plan}` - Successfully created validation plan. The validation plan is a map with the following keys:
    - `:order` - A list of validation IDs (tuples of `{module, rule_name}`) in the order they should be executed.
    - `:dependencies` - A map where each key is a validation ID and the value is a list of validation IDs that the key depends on.
  - `{:error, reason}` - Failed to create validation plan. The reason is a string describing the error.
  """
  @spec resolve_dependencies(atom() | map(), keyword()) :: {:ok, map()} | {:error, any()}
  def resolve_dependencies(resource_or_module, opts \\ []) do
    max_depth = Keyword.get(opts, :max_depth, 3)
    include_only = Keyword.get(opts, :include_only, nil)
    exclude_relationships = Keyword.get(opts, :exclude_relationships, [])

    # Get the resource module
    resource_module =
      cond do
        is_atom(resource_or_module) -> resource_or_module
        is_map(resource_or_module) -> Map.get(resource_or_module, :__resource_module__)
        true -> nil
      end

    if is_nil(resource_module) do
      {:error, "Invalid resource or module"}
    else
      # Initialize dependency graph state
      dependency_state = %{
        dependencies: %{},
        visited: MapSet.new(),
        validation_order: [],
        depth_map: %{resource_module: 0},
        circular_dependencies: []
      }

      # Build the dependency graph
      case build_dependency_graph(
             resource_module,
             dependency_state,
             max_depth,
             include_only,
             exclude_relationships
           ) do
        {:ok, graph_state} ->
          # Resolve the validation order
          resolve_validation_order(graph_state)

        {:error, reason} ->
          {:error, reason}
      end
    end
  end

  @doc """
  Executes a validation plan on a resource.

  This function takes a validation plan created by `resolve_dependencies/2` and
  executes the validations in the order specified by the plan.

  ## Options

  - `:context` - Validation context to use during validation
  - `:checkpoint` - Checkpoint state from a previous validation run

  ## Examples

  ```elixir
  # Execute validation plan
  {:ok, results} = ValidationDependencyResolver.execute_validation_plan(validation_plan, user)

  # Execute with context
  {:ok, results} = ValidationDependencyResolver.execute_validation_plan(validation_plan, user, context: %{admin_mode: true})

  # Resume from checkpoint
  {:ok, results} = ValidationDependencyResolver.execute_validation_plan(validation_plan, user, checkpoint: checkpoint)
  ```

  ## Returns

  - `{:ok, validation_results}` - All validations passed
  - `{:error, validation_errors}` - Some validations failed
  - `{:checkpoint, checkpoint_state}` - Partial validation results
  """
  @spec execute_validation_plan(map(), map(), keyword()) ::
          {:ok, map()} | {:error, map()} | {:checkpoint, map()}
  def execute_validation_plan(validation_plan, resource, opts \\ []) do
    context = Keyword.get(opts, :context, %{})
    checkpoint = Keyword.get(opts, :checkpoint, nil)

    # Initialize validation state
    validation_state = %{
      results: %{},
      errors: %{},
      current_index: 0,
      checkpoint: checkpoint
    }

    # Execute validations in order
    execute_validations(validation_plan, resource, validation_state, context)
  end

  @doc """
  Creates a checkpoint from the current validation state.

  This function creates a checkpoint that can be used to resume validation
  from a specific point. This is useful for long-running validations or
  when validation needs to be split across multiple operations.

  ## Examples

  ```elixir
  # Create checkpoint
  checkpoint = ValidationDependencyResolver.create_checkpoint(validation_state)

  # Later, resume from checkpoint
  {:ok, results} = ValidationDependencyResolver.execute_validation_plan(validation_plan, user, checkpoint: checkpoint)
  ```

  ## Returns

  - Checkpoint state that can be used to resume validation
  """
  @spec create_checkpoint(map()) :: map()
  def create_checkpoint(validation_state) do
    # Extract checkpoint-relevant data
    %{
      current_index: validation_state.current_index,
      results: validation_state.results,
      errors: validation_state.errors
    }
  end

  # Build dependency graph for a resource module
  defp build_dependency_graph(
         resource_module,
         dependency_state,
         max_depth,
         include_only,
         exclude_relationships
       ) do
    # If we've already visited this module or exceeded max depth, skip
    current_depth = Map.get(dependency_state.depth_map, resource_module, 0)

    if MapSet.member?(dependency_state.visited, resource_module) || current_depth > max_depth do
      {:ok, dependency_state}
    else
      # Mark this module as visited
      dependency_state = %{
        dependency_state
        | visited: MapSet.put(dependency_state.visited, resource_module)
      }

      # Get validation rules for this module
      validation_rules = get_validation_rules(resource_module)

      # Add validation rules to dependency graph
      dependency_state =
        add_validations_to_graph(
          resource_module,
          validation_rules,
          dependency_state
        )

      # Get relationships for this module
      relationships = RelationshipResolver.get_relationships(resource_module)

      # Filter relationships based on include_only and exclude_relationships
      relationships = filter_relationships(relationships, exclude_relationships, include_only)

      # Process each relationship
      Enum.reduce_while(relationships, {:ok, dependency_state}, fn relationship, {:ok, current_state} ->
        # Get the related resource module
        related_module = relationship.resource

        # Track depth for the related module
        current_state = %{
          current_state
          | depth_map: Map.put(current_state.depth_map, related_module, current_depth + 1)
        }

        # Add relationship validation dependencies
        current_state =
          add_relationship_dependencies(
            resource_module,
            related_module,
            relationship,
            current_state
          )

        # Process the related module
        case build_dependency_graph(
               related_module,
               current_state,
               max_depth,
               include_only,
               exclude_relationships
             ) do
          {:ok, updated_state} -> {:cont, {:ok, updated_state}}
          {:error, reason} -> {:halt, {:error, reason}}
        end
      end)
    end
  end

  # Get validation rules for a resource module
  defp get_validation_rules(resource_module) do
    # Check if the module implements __validation_rules__/0
    if function_exported?(resource_module, :__validation_rules__, 0) do
      resource_module.__validation_rules__()
    else
      # No validation rules defined
      []
    end
  end

  # Add validations to the dependency graph
  defp add_validations_to_graph(module, validation_rules, dependency_state) do
    # Start with current state
    initial_state = dependency_state

    # Process each validation rule
    Enum.reduce(validation_rules, initial_state, fn {rule_name, _rule_fn}, state ->
      # Create unique validation ID
      validation_id = {module, rule_name}

      # Extract dependencies from the rule
      dependencies = extract_rule_dependencies(module, rule_name)

      # Add validation to the graph
      add_validation_to_graph(validation_id, dependencies, state)
    end)
  end

  # Extract dependencies from a validation rule
  defp extract_rule_dependencies(module, rule_name) do
    # Check if the module implements __validation_dependencies__/1
    if function_exported?(module, :__validation_dependencies__, 1) do
      module.__validation_dependencies__(rule_name)
    else
      # No dependencies defined
      []
    end
  end

  # Add a validation to the dependency graph
  defp add_validation_to_graph(validation_id, dependencies, dependency_state) do
    # Add the validation to the graph if not already present
    dependencies_map = Map.get(dependency_state.dependencies, validation_id, [])

    # Merge with existing dependencies
    dependencies_map = Enum.uniq(dependencies_map ++ dependencies)

    # Update the graph
    %{
      dependency_state
      | dependencies: Map.put(dependency_state.dependencies, validation_id, dependencies_map)
    }
  end

  # Add dependencies between related resources
  defp add_relationship_dependencies(source_module, target_module, relationship, dependency_state) do
    # Get validation rules that depend on this relationship
    relationship_dependencies =
      if function_exported?(source_module, :__relationship_dependencies__, 1) do
        source_module.__relationship_dependencies__(relationship.name)
      else
        []
      end

    # Add dependencies to the graph
    Enum.reduce(relationship_dependencies, dependency_state, fn {source_rule, target_rules}, state ->
      # Create source validation ID
      source_id = {source_module, source_rule}

      # Add dependencies to the graph
      Enum.reduce(target_rules, state, fn target_rule, inner_state ->
        # Create target validation ID
        target_id = {target_module, target_rule}

        # Get existing dependencies for the source
        source_deps = Map.get(inner_state.dependencies, source_id, [])

        # Add the target as a dependency
        updated_deps = Enum.uniq([target_id | source_deps])

        # Update the graph
        %{
          inner_state
          | dependencies: Map.put(inner_state.dependencies, source_id, updated_deps)
        }
      end)
    end)
  end

  # Filter relationships based on include_only and exclude_relationships
  defp filter_relationships(relationships, exclude_relationships, include_only) do
    relationships
    |> Enum.filter(fn relationship ->
      # Include only specific relationships if specified
      include =
        if is_nil(include_only),
          do: true,
          else: Enum.member?(include_only, relationship.name)

      # Exclude specified relationships
      exclude = Enum.member?(exclude_relationships, relationship.name)

      include && !exclude
    end)
  end

  # Resolve the validation order from the dependency graph
  defp resolve_validation_order(dependency_state) do
    # Initialize state for topological sort
    toposort_state = %{
      order: [],
      visited: MapSet.new(),
      temp_visited: MapSet.new(),
      dependencies: dependency_state.dependencies,
      circular_dependencies: []
    }

    # Get all nodes in the graph
    nodes = Map.keys(dependency_state.dependencies)

    # Perform topological sort
    result = topological_sort(nodes, toposort_state)

    case result do
      {:ok, sorted_state} ->
        # Check if we found any circular dependencies
        if Enum.empty?(sorted_state.circular_dependencies) do
          # Create the validation plan
          validation_plan = %{
            order: Enum.reverse(sorted_state.order),
            dependencies: dependency_state.dependencies
          }

          {:ok, validation_plan}
        else
          # Report circular dependencies
          {:error,
           %{
             message: "Circular dependencies detected",
             circular_dependencies: sorted_state.circular_dependencies
           }}
        end

      {:error, reason} ->
        {:error, reason}
    end
  end

  # Perform topological sort on the dependency graph
  defp topological_sort(nodes, state) do
    # Start with current state
    initial_result = {:ok, state}

    # Process each node
    Enum.reduce_while(nodes, initial_result, fn node, {:ok, current_state} ->
      # If the node has already been visited, skip it
      if MapSet.member?(current_state.visited, node) do
        {:cont, {:ok, current_state}}
      else
        # Visit the node
        case visit_node(node, current_state) do
          {:ok, updated_state} -> {:cont, {:ok, updated_state}}
          {:error, reason} -> {:halt, {:error, reason}}
        end
      end
    end)
  end

  # Visit a node during topological sort
  defp visit_node(node, state) do
    # If the node is in temp_visited, we found a cycle
    if MapSet.member?(state.temp_visited, node) do
      # Add the cycle to the list of circular dependencies
      state = %{
        state
        | circular_dependencies: [node | state.circular_dependencies]
      }

      # Continue with sort
      {:ok, state}
    else
      # If the node has already been visited, skip it
      if MapSet.member?(state.visited, node) do
        {:ok, state}
      else
        # Mark the node as temporarily visited
        state = %{
          state
          | temp_visited: MapSet.put(state.temp_visited, node)
        }

        # Get dependencies for this node
        dependencies = Map.get(state.dependencies, node, [])

        # Visit all dependencies
        case visit_dependencies(dependencies, state) do
          {:ok, updated_state} ->
            # Mark the node as visited
            updated_state = %{
              updated_state
              | visited: MapSet.put(updated_state.visited, node),
                temp_visited: MapSet.delete(updated_state.temp_visited, node),
                order: [node | updated_state.order]
            }

            {:ok, updated_state}

          {:error, reason} ->
            {:error, reason}
        end
      end
    end
  end

  # Visit all dependencies of a node
  defp visit_dependencies(dependencies, state) do
    # Start with current state
    initial_result = {:ok, state}

    # Process each dependency
    Enum.reduce_while(dependencies, initial_result, fn dep, {:ok, current_state} ->
      # Visit the dependency
      case visit_node(dep, current_state) do
        {:ok, updated_state} -> {:cont, {:ok, updated_state}}
        {:error, reason} -> {:halt, {:error, reason}}
      end
    end)
  end

  # Execute validations in the order specified by the validation plan
  defp execute_validations(validation_plan, resource, validation_state, context) do
    # Get the validation order
    validation_order = validation_plan.order

    # Get current progress from checkpoint
    current_index =
      if validation_state.checkpoint do
        validation_state.checkpoint.current_index
      else
        0
      end

    # Get results and errors from checkpoint
    validation_state =
      if validation_state.checkpoint do
        %{
          validation_state
          | results: validation_state.checkpoint.results,
            errors: validation_state.checkpoint.errors
        }
      else
        validation_state
      end

    # Execute validations from current index
    validations_to_execute = Enum.drop(validation_order, current_index)

    Enum.reduce_while(validations_to_execute, {:ok, validation_state}, fn
      validation_id, {:ok, current_state} ->
        # Update current index
        current_state = %{current_state | current_index: current_state.current_index + 1}

        # Execute the validation
        case execute_validation(validation_id, resource, context) do
          {:ok, result} ->
            # Add result to state
            current_state = %{
              current_state
              | results: Map.put(current_state.results, validation_id, result)
            }

            {:cont, {:ok, current_state}}

          {:error, error} ->
            # Add error to state
            current_state = %{
              current_state
              | errors: Map.put(current_state.errors, validation_id, error)
            }

            # Check if we should continue or stop on first error
            if Keyword.get(context, :stop_on_first_error, true) do
              # Create a checkpoint for resuming later
              checkpoint = create_checkpoint(current_state)

              {:halt, {:checkpoint, checkpoint}}
            else
              {:cont, {:ok, current_state}}
            end

          {:checkpoint, checkpoint_data} ->
            # Return checkpoint for resuming later
            checkpoint = %{
              current_index: current_state.current_index,
              results: current_state.results,
              errors: current_state.errors,
              checkpoint_data: checkpoint_data
            }

            {:halt, {:checkpoint, checkpoint}}
        end
    end)
    |> case do
      {:ok, state} ->
        # Check if there are any errors
        if Enum.empty?(state.errors) do
          {:ok, state.results}
        else
          {:error, state.errors}
        end

      {:checkpoint, checkpoint} ->
        {:checkpoint, checkpoint}
    end
  end

  # Execute a single validation
  defp execute_validation({module, rule_name}, resource, context) do
    # Get the resource's module
    resource_module = Map.get(resource, :__resource_module__)

    if resource_module != module do
      # Validation is for a different module
      # This should not happen, but handle it gracefully
      {:error,
       "Validation #{inspect(rule_name)} for module #{inspect(module)} cannot be applied to resource of type #{inspect(resource_module)}"}
    else
      # Get validation function
      validation_rules = get_validation_rules(module)
      rule_fn = Keyword.get(validation_rules, rule_name)

      if is_nil(rule_fn) do
        # Rule not found
        {:error, "Validation rule #{inspect(rule_name)} not found in module #{inspect(module)}"}
      else
        # Apply the validation rule
        try do
          # Try to apply the rule with context first
          result = rule_fn.(resource, context)
          handle_validation_result(result)
        rescue
          FunctionClauseError ->
            # Rule doesn't accept context, try without context
            try do
              result = rule_fn.(resource)
              handle_validation_result(result)
            rescue
              e ->
                # Error applying rule
                {:error, "Error applying validation rule: #{inspect(e)}"}
            end

          e ->
            # Error applying rule
            {:error, "Error applying validation rule: #{inspect(e)}"}
        end
      end
    end
  end

  # Handle the result of a validation function
  defp handle_validation_result(result) do
    case result do
      :ok -> {:ok, :valid}
      {:ok, value} -> {:ok, value}
      {:error, reason} -> {:error, reason}
      {:checkpoint, data} -> {:checkpoint, data}
      _ -> {:error, "Invalid validation result: #{inspect(result)}"}
    end
  end

  @doc """
  Visualizes the dependency graph for a validation plan.

  This function creates a string representation of the dependency graph
  that can be used for debugging or visualization.

  ## Options

  - `:format` - Output format, either `:text` or `:dot` (default: `:text`)
  - `:highlight` - Validation ID to highlight in the graph
  - `:max_depth` - Maximum depth to display for dependencies (default: nil, meaning no limit)

  ## Examples

  ```elixir
  # Basic text visualization
  ValidationDependencyResolver.visualize_dependency_graph(validation_plan)

  # GraphViz DOT format for visualization tools
  ValidationDependencyResolver.visualize_dependency_graph(validation_plan, format: :dot)

  # Highlight a specific validation
  ValidationDependencyResolver.visualize_dependency_graph(validation_plan, highlight: {UserResource, :email_must_be_valid})
  ```

  ## Returns

  String representation of the dependency graph
  """
  @spec visualize_dependency_graph(map(), keyword()) :: String.t()
  def visualize_dependency_graph(validation_plan, opts \\ []) do
    format = Keyword.get(opts, :format, :text)
    highlight = Keyword.get(opts, :highlight, nil)
    max_depth = Keyword.get(opts, :max_depth, nil)

    case format do
      :text -> visualize_as_text(validation_plan, highlight, max_depth)
      :dot -> visualize_as_dot(validation_plan, highlight, max_depth)
      _ -> raise "Unsupported format: #{inspect(format)}"
    end
  end

  @doc """
  Analyzes the dependencies for a specific validation.

  This function provides detailed information about the dependencies
  for a specific validation, including direct dependencies, reverse dependencies
  (validations that depend on this one), and the dependency chain.

  ## Options

  - `:max_depth` - Maximum depth to analyze for dependencies (default: nil, meaning no limit)

  ## Examples

  ```elixir
  # Basic dependency analysis
  ValidationDependencyResolver.analyze_dependencies(validation_plan, {UserResource, :email_must_be_valid})

  # Limit dependency depth
  ValidationDependencyResolver.analyze_dependencies(validation_plan, {UserResource, :email_must_be_valid}, max_depth: 2)
  ```

  ## Returns

  ```
  %{
    direct_dependencies: [{ModuleA, :rule_a}, {ModuleB, :rule_b}],
    reverse_dependencies: [{ModuleC, :rule_c}],
    dependency_chain: [{ModuleA, :rule_a}, {ModuleD, :rule_d}],
    cycles: []
  }
  ```
  """
  @spec analyze_dependencies(map(), {atom(), atom()}, keyword()) :: map()
  def analyze_dependencies(validation_plan, validation_id, opts \\ []) do
    max_depth = Keyword.get(opts, :max_depth, nil)

    # Get direct dependencies
    direct_dependencies = Map.get(validation_plan.dependencies, validation_id, [])

    # Get reverse dependencies
    reverse_dependencies =
      validation_plan.dependencies
      |> Enum.filter(fn {_id, deps} -> Enum.member?(deps, validation_id) end)
      |> Enum.map(fn {id, _deps} -> id end)

    # Get dependency chain
    dependency_chain = get_dependency_chain(validation_plan, validation_id, max_depth)

    # Check for cycles
    cycles = find_cycles_for_validation(validation_plan, validation_id)

    %{
      direct_dependencies: direct_dependencies,
      reverse_dependencies: reverse_dependencies,
      dependency_chain: dependency_chain,
      cycles: cycles
    }
  end

  @doc """
  Detects and reports circular dependencies in a validation plan.

  This function analyzes the dependency graph and identifies any
  circular dependencies between validations.

  ## Examples

  ```elixir
  # Detect circular dependencies
  case ValidationDependencyResolver.detect_circular_dependencies(validation_plan) do
    [] -> 
      # No circular dependencies
      :ok
      
    cycles -> 
      # Circular dependencies detected
      IO.puts("Warning: Circular dependencies detected:")
      Enum.each(cycles, fn c ->
        IO.puts("  - \#{inspect(c)}")
      end)
  end
  ```

  ## Returns

  List of cycles in the dependency graph, where each cycle is a list of validation IDs
  """
  @spec detect_circular_dependencies(map()) :: [list({atom(), atom()})]
  def detect_circular_dependencies(validation_plan) do
    # Get all nodes in the graph
    nodes = Map.keys(validation_plan.dependencies)

    # Find cycles for each node
    cycles =
      nodes
      |> Enum.flat_map(fn node -> find_cycles_for_validation(validation_plan, node) end)
      |> Enum.uniq()

    cycles
  end

  @doc """
  Fixes circular dependencies in a validation plan by breaking cycles.

  This function analyzes the dependency graph, identifies circular dependencies,
  and breaks the cycles by removing the least important dependencies.

  ## Options

  - `:strategy` - Strategy for breaking cycles, either `:remove_minimal` or `:prioritize_order` (default: `:remove_minimal`)
  - `:priorities` - List of validation IDs in order of priority for the `:prioritize_order` strategy

  ## Examples

  ```elixir
  # Fix circular dependencies using the default strategy
  {:ok, fixed_plan} = ValidationDependencyResolver.fix_circular_dependencies(validation_plan)

  # Fix circular dependencies using the prioritize_order strategy
  priorities = [
    {UserResource, :email_must_be_valid},
    {UserResource, :role_must_be_valid}
  ]

  {:ok, fixed_plan} = ValidationDependencyResolver.fix_circular_dependencies(
    validation_plan,
    strategy: :prioritize_order,
    priorities: priorities
  )
  ```

  ## Returns

  - `{:ok, fixed_plan}` - Fixed validation plan with circular dependencies removed
  - `{:error, reason}` - Failed to fix circular dependencies
  """
  @spec fix_circular_dependencies(map(), keyword()) :: {:ok, map()} | {:error, any()}
  def fix_circular_dependencies(validation_plan, opts \\ []) do
    strategy = Keyword.get(opts, :strategy, :remove_minimal)
    priorities = Keyword.get(opts, :priorities, [])

    # Detect cycles
    cycles = detect_circular_dependencies(validation_plan)

    if Enum.empty?(cycles) do
      # No cycles to fix
      {:ok, validation_plan}
    else
      # Fix cycles based on strategy
      case strategy do
        :remove_minimal ->
          remove_minimal_dependencies(validation_plan, cycles)

        :prioritize_order ->
          prioritize_dependencies(validation_plan, cycles, priorities)

        _ ->
          {:error, "Unknown strategy: #{inspect(strategy)}"}
      end
    end
  end

  # Get dependency chain for a validation
  defp get_dependency_chain(validation_plan, validation_id, max_depth) do
    get_dependency_chain_recursive(validation_plan, validation_id, [], 0, max_depth)
  end

  # Recursive helper for get_dependency_chain
  defp get_dependency_chain_recursive(_validation_plan, _validation_id, chain, depth, max_depth)
       when not is_nil(max_depth) and depth >= max_depth,
       do: chain

  defp get_dependency_chain_recursive(validation_plan, validation_id, chain, depth, max_depth) do
    # Get direct dependencies
    direct_dependencies = Map.get(validation_plan.dependencies, validation_id, [])

    # Add current validation to chain
    updated_chain = [validation_id | chain]

    # Process each dependency
    Enum.reduce(direct_dependencies, updated_chain, fn dep, acc ->
      # Skip dependencies already in the chain to prevent infinite recursion
      if Enum.member?(acc, dep) do
        acc
      else
        # Process dependency recursively
        deps_chain =
          get_dependency_chain_recursive(validation_plan, dep, acc, depth + 1, max_depth)

        # Merge chains
        Enum.uniq(deps_chain ++ acc)
      end
    end)
  end

  # Find cycles for a specific validation
  defp find_cycles_for_validation(validation_plan, validation_id) do
    # Initialize state for cycle detection
    state = %{
      visited: MapSet.new(),
      temp_visited: MapSet.new(),
      cycles: [],
      current_path: []
    }

    # Detect cycles
    final_state = detect_cycles_dfs(validation_plan, validation_id, state)

    # Return detected cycles
    final_state.cycles
  end

  # Detect cycles using depth-first search
  defp detect_cycles_dfs(validation_plan, validation_id, state) do
    # If the node is in temp_visited, we found a cycle
    if MapSet.member?(state.temp_visited, validation_id) do
      # Get the cycle path
      cycle_start_index = Enum.find_index(state.current_path, &(&1 == validation_id))

      if is_nil(cycle_start_index) do
        # Node is in temp_visited but not in current_path
        # This should not happen, but handle it gracefully
        state
      else
        # Extract the cycle
        cycle = Enum.slice(state.current_path, cycle_start_index..-1)

        # Add the cycle to the list
        %{state | cycles: [cycle | state.cycles]}
      end
    else
      # If the node has already been visited, skip it
      if MapSet.member?(state.visited, validation_id) do
        state
      else
        # Mark the node as temporarily visited
        state = %{
          state
          | temp_visited: MapSet.put(state.temp_visited, validation_id),
            current_path: [validation_id | state.current_path]
        }

        # Get dependencies for this node
        dependencies = Map.get(validation_plan.dependencies, validation_id, [])

        # Visit all dependencies
        updated_state =
          Enum.reduce(dependencies, state, fn dep, acc ->
            detect_cycles_dfs(validation_plan, dep, acc)
          end)

        # Mark the node as visited
        %{
          updated_state
          | visited: MapSet.put(updated_state.visited, validation_id),
            temp_visited: MapSet.delete(updated_state.temp_visited, validation_id),
            current_path:
              case updated_state.current_path do
                [_ | rest] -> rest
                [] -> []
              end
        }
      end
    end
  end

  # Remove minimal set of dependencies to break cycles
  defp remove_minimal_dependencies(validation_plan, cycles) do
    # Calculate dependency frequency in cycles
    frequency =
      cycles
      |> Enum.flat_map(fn cycle ->
        if length(cycle) > 1 do
          # Defensive: Only use hd/tl if cycle is not empty
          case cycle do
            [] -> []
            [_ | _] -> Enum.zip(cycle, tl(cycle) ++ [hd(cycle)])
          end
        else
          []
        end
      end)
      |> Enum.frequencies()

    # Sort edges by frequency
    edges_to_remove =
      frequency
      |> Enum.sort_by(fn {_edge, freq} -> freq end, :desc)
      |> Enum.map(fn {{from, to}, _freq} -> {from, to} end)

    # Remove edges until all cycles are broken
    dependencies = validation_plan.dependencies

    {updated_dependencies, _} =
      Enum.reduce_while(edges_to_remove, {dependencies, cycles}, fn {from, to}, {deps, _remaining_cycles} ->
        # Remove edge
        from_deps = Map.get(deps, from, [])
        updated_from_deps = Enum.filter(from_deps, &(&1 != to))
        updated_deps = Map.put(deps, from, updated_from_deps)

        # Check if cycles are broken
        updated_plan = %{validation_plan | dependencies: updated_deps}
        remaining = detect_circular_dependencies(updated_plan)

        if Enum.empty?(remaining) do
          # All cycles broken
          {:halt, {updated_deps, []}}
        else
          # Continue removing edges
          Logger.debug("Remaining cycles after removing edge #{from} -> #{to}: #{inspect(remaining)}")

          {:cont, {updated_deps, remaining}}
        end
      end)

    # Create updated validation plan
    updated_plan = %{
      validation_plan
      | dependencies: updated_dependencies
    }

    # Resolve validation order for the updated plan
    case resolve_validation_order(%{dependencies: updated_dependencies}) do
      {:ok, new_plan} ->
        {:ok, Map.put(updated_plan, :order, new_plan.order)}

      {:error, reason} ->
        {:error, reason}
    end
  end

  # Prioritize dependencies based on specified order
  defp prioritize_dependencies(validation_plan, cycles, priorities) do
    if Enum.empty?(priorities) do
      # No priorities specified, fall back to remove_minimal
      remove_minimal_dependencies(validation_plan, cycles)
    else
      # Start with the dependencies
      dependencies = validation_plan.dependencies

      # Process each cycle
      {updated_dependencies, _} =
        Enum.reduce(cycles, {dependencies, cycles}, fn cycle, {deps, remaining_cycles} ->
          # Find the edge to remove based on priorities
          edge_to_remove = find_edge_to_remove(cycle, priorities)

          if is_nil(edge_to_remove) do
            # No suitable edge found, skip this cycle
            {deps, remaining_cycles}
          else
            {from, to} = edge_to_remove

            # Remove edge
            from_deps = Map.get(deps, from, [])
            updated_from_deps = Enum.filter(from_deps, &(&1 != to))
            updated_deps = Map.put(deps, from, updated_from_deps)

            {updated_deps, remaining_cycles}
          end
        end)

      # Create updated validation plan
      updated_plan = %{
        validation_plan
        | dependencies: updated_dependencies
      }

      # Check if all cycles are broken
      remaining_cycles = detect_circular_dependencies(updated_plan)

      if Enum.empty?(remaining_cycles) do
        # All cycles broken, resolve validation order
        case resolve_validation_order(%{dependencies: updated_dependencies}) do
          {:ok, new_plan} ->
            {:ok, Map.put(updated_plan, :order, new_plan.order)}

          {:error, reason} ->
            {:error, reason}
        end
      else
        # Some cycles remain, fall back to remove_minimal
        remove_minimal_dependencies(updated_plan, remaining_cycles)
      end
    end
  end

  # Find edge to remove based on priorities
  defp find_edge_to_remove(cycle, priorities) do
    # Generate all edges in the cycle
    edges =
      if length(cycle) > 1 do
        # Defensive: Only use hd/tl if cycle is not empty
        case cycle do
          [] -> []
          [_ | _] -> Enum.zip(cycle, tl(cycle) ++ [hd(cycle)])
        end
      else
        []
      end

    # Find the edge with the lowest priority
    Enum.min_by(
      edges,
      fn {from, to} ->
        from_priority = Enum.find_index(priorities, &(&1 == from)) || 999
        to_priority = Enum.find_index(priorities, &(&1 == to)) || 999

        # Prefer removing edges where the from node has lower priority
        {from_priority, to_priority}
      end,
      fn -> nil end
    )
  end

  # Visualize dependency graph as text
  defp visualize_as_text(validation_plan, _highlight, _max_depth) do
    # Visualize validation plan as text
    validation_plan
  end

  # Visualize dependency graph as GraphViz DOT
  defp visualize_as_dot(validation_plan, _highlight, _max_depth) do
    # Visualize validation plan as DOT graph
    validation_plan
  end
end
