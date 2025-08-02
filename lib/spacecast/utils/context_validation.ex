defmodule Spacecast.Utils.ContextValidation do
  @moduledoc """
  Context-aware validation for resources and their relationships.

  This module provides functionality for validating resources in the context
  of their relationships, allowing for complex validation rules that span
  multiple resources.

  ## Features

  - **Validation Context Propagation**: Pass validation context between related resources
  - **Parent-Child Validation Rules**: Define validation rules based on parent-child relationships
  - **Cross-Resource Validation Patterns**: Validate resources based on relationships with other resources

  ## Examples

  ```elixir
  # Validate a resource and all its related resources with context
  {:ok, _} = ContextValidation.validate_deep(user, context: %{allowed_roles: ["admin", "editor"]})

  # Validate with specific validation rules
  {:ok, _} = ContextValidation.validate_with_rules(user, [:team_member_role, :access_permissions])
  ```
  """

  alias Spacecast.Utils.RelationshipResolver

  @doc """
  Validates a resource and all of its related resources with context.

  ## Options

  - `:context` - Validation context to propagate to related resources
  - `:max_depth` - Maximum depth for traversing relationships (default: 3)
  - `:exclude_relationships` - List of relationship names to exclude from validation
  - `:include_only` - List of relationship names to include in validation (excludes all others)
  - `:rules` - List of validation rule names to apply (default: all rules)

  ## Examples

  ```elixir
  # Basic validation
  {:ok, _} = ContextValidation.validate_deep(user)

  # Validation with context
  {:ok, _} = ContextValidation.validate_deep(user, context: %{admin_mode: true})

  # Validation with limited depth
  {:ok, _} = ContextValidation.validate_deep(user, max_depth: 2)

  # Validation with specific relationships
  {:ok, _} = ContextValidation.validate_deep(user, include_only: [:team, :posts])
  ```

  ## Returns

  - `{:ok, validation_results}` - Validation succeeded
  - `{:error, validation_errors}` - Validation failed
  """
  @spec validate_deep(map(), keyword()) :: {:ok, map()} | {:error, map()}
  def validate_deep(resource, opts \\ []) do
    context = Keyword.get(opts, :context, %{})
    max_depth = Keyword.get(opts, :max_depth, 3)
    exclude_relationships = Keyword.get(opts, :exclude_relationships, [])
    include_only = Keyword.get(opts, :include_only, nil)
    rules = Keyword.get(opts, :rules, nil)

    # Initialize validation state
    validation_state = %{
      visited: MapSet.new(),
      errors: %{},
      depth_map: %{resource: 0},
      context: context
    }

    # Start the validation process
    do_validate_deep(
      resource,
      validation_state,
      max_depth,
      exclude_relationships,
      include_only,
      rules
    )
  end

  @doc """
  Validates a resource with specific validation rules.

  ## Options

  - `:context` - Validation context to use during validation
  - `:rules` - List of validation rule names to apply (required)

  ## Examples

  ```elixir
  # Validate with specific rules
  {:ok, _} = ContextValidation.validate_with_rules(user, rules: [:team_member_role, :access_permissions])

  # Validate with context
  {:ok, _} = ContextValidation.validate_with_rules(user,
    rules: [:team_member_role],
    context: %{allowed_roles: ["admin", "editor"]}
  )

  # Validate with both rules and context
  {:ok, _} = ContextValidation.validate_with_rules(user,
    rules: [:team_member_role, :access_permissions],
    context: %{allowed_roles: ["admin", "editor"]}
  )
  ```

  ## Returns

  - `{:ok, resource}` - Validation succeeded
  - `{:error, errors}` - Validation failed
  """
  @spec validate_with_rules(map(), keyword()) :: {:ok, map()} | {:error, list()}
  def validate_with_rules(resource, opts \\ []) do
    context = Keyword.get(opts, :context, %{})
    rules = Keyword.get(opts, :rules)

    if is_nil(rules) do
      {:error, "No validation rules specified"}
    else
      do_validate_with_rules(resource, rules, context)
    end
  end

  # Private implementation for deep validation
  defp do_validate_deep(
         resource,
         validation_state,
         max_depth,
         exclude_relationships,
         include_only,
         rules
       ) do
    # Get resource identifier for tracking visited resources
    resource_id = get_resource_id(resource)
    current_depth = Map.get(validation_state.depth_map, resource, 0)

    # If we've already visited this resource or exceeded max depth, skip validation
    if MapSet.member?(validation_state.visited, resource_id) || current_depth > max_depth do
      {:ok, validation_state}
    else
      # Mark this resource as visited
      validation_state = %{
        validation_state
        | visited: MapSet.put(validation_state.visited, resource_id)
      }

      # Validate the resource itself
      case validate_resource(resource, rules, validation_state.context) do
        {:ok, _} ->
          # Resource is valid, proceed to related resources
          validate_related_resources(
            resource,
            validation_state,
            max_depth,
            exclude_relationships,
            include_only,
            rules
          )

        {:error, errors} ->
          # Resource is invalid, record errors and continue with related resources
          validation_state = %{
            validation_state
            | errors: Map.put(validation_state.errors, resource_id, errors)
          }

          validate_related_resources(
            resource,
            validation_state,
            max_depth,
            exclude_relationships,
            include_only,
            rules
          )
      end
    end
  end

  # Validate related resources
  defp validate_related_resources(
         resource,
         validation_state,
         max_depth,
         exclude_relationships,
         include_only,
         rules
       ) do
    # Get the resource module
    resource_module = Map.get(resource, :__resource_module__)

    if is_nil(resource_module) do
      # Resource doesn't have a module, can't get relationships
      {:ok, validation_state}
    else
      # Get relationships for this resource
      relationships = RelationshipResolver.get_relationships(resource_module)

      # Filter relationships based on include_only and exclude_relationships
      relationships = filter_relationships(relationships, exclude_relationships, include_only)

      # Start with current validation state
      initial_result = {:ok, validation_state}

      # Validate each relationship and accumulate results
      Enum.reduce(
        relationships,
        initial_result,
        &process_relationship_validation(
          &1,
          &2,
          resource,
          max_depth,
          exclude_relationships,
          include_only,
          rules
        )
      )
    end
  end

  defp process_relationship_validation(
         relationship,
         acc,
         resource,
         max_depth,
         exclude_relationships,
         include_only,
         rules
       ) do
    case acc do
      {:ok, current_state} ->
        validate_related_resource(
          resource,
          relationship,
          current_state,
          max_depth,
          exclude_relationships,
          include_only,
          rules
        )

      error ->
        error
    end
  end

  # Validate a specific related resource
  defp validate_related_resource(
         resource,
         relationship,
         validation_state,
         max_depth,
         exclude_relationships,
         include_only,
         rules
       ) do
    # Get the relationship name
    relationship_name = relationship.name

    # Resolve the relationship
    case RelationshipResolver.resolve_relationship(resource, relationship_name) do
      {:ok, nil} ->
        # No related resource, nothing to validate
        {:ok, validation_state}

      {:ok, related_items} when is_list(related_items) ->
        # List of related resources, validate each one
        validate_related_items(
          related_items,
          resource,
          relationship,
          validation_state,
          max_depth,
          exclude_relationships,
          include_only,
          rules
        )

      {:ok, related} ->
        # Single related resource, validate it
        # Track the relationship depth
        current_depth = Map.get(validation_state.depth_map, resource, 0)

        validation_state = %{
          validation_state
          | depth_map: Map.put(validation_state.depth_map, related, current_depth + 1)
        }

        # Prepare the context for the related resource
        context =
          prepare_related_context(validation_state.context, resource, relationship, related)

        validation_state = %{validation_state | context: context}

        # Validate the related resource
        do_validate_deep(
          related,
          validation_state,
          max_depth,
          exclude_relationships,
          include_only,
          rules
        )

      {:error, _reason} ->
        # Error resolving relationship, continue with current state
        {:ok, validation_state}
    end
  end

  # Validate a list of related items
  defp validate_related_items(
         related_items,
         resource,
         relationship,
         validation_state,
         max_depth,
         exclude_relationships,
         include_only,
         rules
       ) do
    # Filter out nil items
    related_items = Enum.filter(related_items, &(&1 != nil))

    # Start with current validation state
    initial_result = {:ok, validation_state}

    # Validate each item and accumulate results
    Enum.reduce(related_items, initial_result, fn related, acc ->
      case acc do
        {:ok, current_state} ->
          # Track the relationship depth
          current_depth = Map.get(current_state.depth_map, resource, 0)

          current_state = %{
            current_state
            | depth_map: Map.put(current_state.depth_map, related, current_depth + 1)
          }

          # Prepare the context for the related resource
          context =
            prepare_related_context(current_state.context, resource, relationship, related)

          current_state = %{current_state | context: context}

          # Validate the related resource
          do_validate_deep(
            related,
            current_state,
            max_depth,
            exclude_relationships,
            include_only,
            rules
          )

        error ->
          error
      end
    end)
  end

  # Prepare the context for a related resource
  defp prepare_related_context(context, parent, relationship, child) do
    # Add relationship metadata to the context
    context =
      Map.put(context, :relationship, %{
        name: relationship,
        parent: parent,
        child: child
      })

    context
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

  # Validate a resource with rules
  defp do_validate_with_rules(resource, rules, context) do
    # Get the resource module
    resource_module = Map.get(resource, :__resource_module__)

    if is_nil(resource_module) do
      {:error, ["Resource does not have a __resource_module__ attribute"]}
    else
      # Get validation rules for the resource
      resource_rules = get_validation_rules(resource_module)

      # Filter rules to include only those specified
      filtered_rules = filter_rules(resource_rules, rules)

      # Validate with each rule
      errors = Enum.flat_map(filtered_rules, &process_rule_result(&1, resource, context))

      if Enum.empty?(errors) do
        {:ok, resource}
      else
        {:error, errors}
      end
    end
  end

  defp process_rule_result({rule_name, rule_fn}, resource, context) do
    case apply_rule(rule_fn, resource, context) do
      :ok -> []
      {:error, message} -> [{rule_name, message}]
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

  # Filter rules based on specified rule names
  defp filter_rules(resource_rules, rule_names) do
    if is_nil(rule_names) do
      # If no rules specified, use all rules
      resource_rules
    else
      # Filter rules by name
      Enum.filter(resource_rules, fn {rule_name, _} ->
        Enum.member?(rule_names, rule_name)
      end)
    end
  end

  # Apply a validation rule to a resource
  defp apply_rule(rule_fn, resource, context) do
    # Try to apply the rule with context first
    try do
      rule_fn.(resource, context)
    rescue
      FunctionClauseError ->
        # Rule doesn't accept context, try without context
        try do
          rule_fn.(resource)
        rescue
          e ->
            # Error applying rule
            {:error, "Error applying validation rule: #{inspect(e)}"}
        end
    end
  end

  # Validate a resource
  defp validate_resource(resource, rules, context) do
    # Get the resource module
    resource_module = Map.get(resource, :__resource_module__)

    if is_nil(resource_module) do
      # Resource doesn't have a module, skip validation
      {:ok, resource}
    else
      # Validate with specified rules or all rules
      do_validate_with_rules(resource, rules, context)
    end
  end

  # Get a unique identifier for a resource
  defp get_resource_id(resource) do
    if Map.has_key?(resource, :id) do
      # Use ID if available
      {Map.get(resource, :__resource_module__), Map.get(resource, :id)}
    else
      # Use the resource itself as a last resort
      # This might not be ideal for memory usage, but ensures uniqueness
      resource
    end
  end
end
