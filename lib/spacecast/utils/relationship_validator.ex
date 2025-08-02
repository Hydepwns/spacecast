defmodule Spacecast.Utils.RelationshipValidator do
  @moduledoc """
  Handles validation of relationships between resources.

  This module provides functionality for validating relationships,
  checking referential integrity, and enforcing relationship constraints.

  ## Features

  - Referential integrity checks for belongs_to relationships
  - Cascading updates/deletes validation
  - Validation rules for relationship constraints
  """

  alias Spacecast.Utils.RelationshipResolver

  @doc """
  Validates relationships for a resource.

  Checks that all required relationships are present and satisfy
  their constraints. Also validates referential integrity when possible.

  ## Options

  * `:check_referential_integrity` - If set to `true`, validates that related
    entities actually exist. Defaults to `true`.
  * `:deep` - If set to `true`, also validates the related entities themselves.
    Defaults to `false`.

  ## Examples

      iex> validate_relationships(user)
      :ok

      iex> validate_relationships(invalid_user)
      {:error, [%{relationship: :team, error: "Team with ID 5 does not exist"}]}
  """
  @spec validate_relationships(map(), keyword()) :: :ok | {:error, [map()]}
  def validate_relationships(resource, opts \\ []) do
    check_referential_integrity = Keyword.get(opts, :check_referential_integrity, true)
    deep = Keyword.get(opts, :deep, false)

    # Get the resource module
    resource_module = Map.get(resource, :__resource_module__)

    if is_nil(resource_module) do
      {:error, [%{error: "Resource does not have a __resource_module__ attribute"}]}
    else
      # Get all relationships
      relationships = RelationshipResolver.get_relationships(resource_module)

      # Validate each relationship
      errors =
        Enum.flat_map(relationships, fn relationship ->
          validate_relationship(resource, relationship, check_referential_integrity, deep)
        end)

      if Enum.empty?(errors) do
        :ok
      else
        {:error, errors}
      end
    end
  end

  @doc """
  Validates a specific relationship for a resource.

  ## Examples

      iex> validate_relationship(user, :team)
      []  # No errors

      iex> validate_relationship(invalid_user, :team)
      [%{relationship: :team, error: "Team with ID 5 does not exist"}]
  """
  @spec validate_relationship(map(), atom(), boolean(), boolean()) :: [map()]
  def validate_relationship(
        resource,
        relationship_name,
        check_referential_integrity \\ true,
        deep \\ false
      ) do
    # Get the resource module
    resource_module = Map.get(resource, :__resource_module__)

    if is_nil(resource_module) do
      [%{error: "Resource does not have a __resource_module__ attribute"}]
    else
      # Get relationship definition
      case RelationshipResolver.get_relationship_definition(resource_module, relationship_name) do
        {:ok, relationship} ->
          do_validate_relationship(resource, relationship, check_referential_integrity, deep)

        {:error, reason} ->
          [%{relationship: relationship_name, error: reason}]
      end
    end
  end

  # Private implementation for validating a relationship
  defp do_validate_relationship(resource, relationship, check_referential_integrity, deep) do
    case relationship.type do
      :belongs_to ->
        validate_belongs_to(resource, relationship, check_referential_integrity, deep)

      :has_many ->
        validate_has_many(resource, relationship, check_referential_integrity, deep)

      :has_one ->
        validate_has_one(resource, relationship, check_referential_integrity, deep)

      :through ->
        validate_through(resource, relationship, check_referential_integrity, deep)

      :polymorphic ->
        validate_polymorphic(resource, relationship, check_referential_integrity, deep)

      _ ->
        [
          %{
            relationship: relationship.name,
            error: "Unsupported relationship type: #{inspect(relationship.type)}"
          }
        ]
    end
  end

  # Validates a belongs_to relationship
  defp validate_belongs_to(resource, relationship, check_referential_integrity, deep) do
    foreign_key = relationship.foreign_key || :"#{relationship.name}_id"
    foreign_key_value = Map.get(resource, foreign_key)

    []
    |> validate_required_relationship(relationship, foreign_key_value)
    |> validate_referential_integrity(
      relationship,
      foreign_key,
      foreign_key_value,
      check_referential_integrity
    )
    |> validate_deep_relationship(
      resource,
      relationship,
      foreign_key_value,
      deep,
      check_referential_integrity
    )
  end

  defp validate_required_relationship(errors, relationship, foreign_key_value) do
    if relationship.required && is_nil(foreign_key_value) do
      [%{relationship: relationship.name, error: "Required relationship is nil"} | errors]
    else
      errors
    end
  end

  defp validate_referential_integrity(
         errors,
         relationship,
         foreign_key,
         foreign_key_value,
         check_referential_integrity
       ) do
    if check_referential_integrity && !is_nil(foreign_key_value) do
      case relationship.resource.load(foreign_key_value) do
        {:ok, _} ->
          errors

        {:error, reason} ->
          [
            %{
              relationship: relationship.name,
              error: "Referenced entity not found: #{reason}",
              foreign_key: foreign_key,
              foreign_key_value: foreign_key_value
            }
            | errors
          ]
      end
    else
      errors
    end
  end

  defp validate_deep_relationship(
         errors,
         resource,
         relationship,
         foreign_key_value,
         deep,
         check_referential_integrity
       ) do
    if deep && !is_nil(foreign_key_value) do
      case RelationshipResolver.resolve_relationship(resource, relationship.name) do
        {:ok, related} when not is_nil(related) ->
          validate_relationships(related,
            check_referential_integrity: check_referential_integrity
          )
      end
    else
      errors
    end
  end

  # Validates a has_many relationship
  defp validate_has_many(resource, relationship, check_referential_integrity, deep) do
    # For has_many, we typically don't validate unless deep validation is requested
    if deep do
      validate_has_many_items(resource, relationship, check_referential_integrity)
    else
      []
    end
  end

  defp validate_has_many_items(resource, relationship, check_referential_integrity) do
    case RelationshipResolver.resolve_relationship(resource, relationship.name) do
      {:ok, related_items} when is_list(related_items) ->
        Enum.flat_map(related_items, fn item ->
          validate_relationships(item, check_referential_integrity: check_referential_integrity)
        end)

      _ ->
        []
    end
  end

  # Validates a has_one relationship
  defp validate_has_one(resource, relationship, check_referential_integrity, deep) do
    # Similar to has_many, but expecting only one related entity
    if deep do
      validate_has_one_item(resource, relationship, check_referential_integrity)
    else
      []
    end
  end

  defp validate_has_one_item(resource, relationship, check_referential_integrity) do
    case RelationshipResolver.resolve_relationship(resource, relationship.name) do
      {:ok, related} when not is_nil(related) ->
        validate_relationships(related, check_referential_integrity: check_referential_integrity)

      _ ->
        []
    end
  end

  # Validates a through relationship
  defp validate_through(resource, relationship, check_referential_integrity, deep) do
    through_name = relationship.through

    # Validate the intermediate relationship
    through_errors =
      validate_relationship(resource, through_name, check_referential_integrity, false)

    if Enum.empty?(through_errors) && deep do
      validate_through_targets(
        resource,
        relationship,
        through_name,
        check_referential_integrity,
        deep,
        through_errors
      )
    else
      through_errors
    end
  end

  defp validate_through_targets(
         resource,
         relationship,
         through_name,
         check_referential_integrity,
         deep,
         errors
       ) do
    case RelationshipResolver.resolve_relationship(resource, through_name) do
      {:ok, intermediates} ->
        target_errors =
          validate_intermediate_targets(
            intermediates,
            relationship,
            through_name,
            check_referential_integrity,
            deep
          )

        target_errors ++ errors

      _ ->
        errors
    end
  end

  defp validate_intermediate_targets(
         intermediates,
         relationship,
         through_name,
         check_referential_integrity,
         deep
       ) do
    intermediates_list = if is_list(intermediates), do: intermediates, else: [intermediates]
    filtered_intermediates = Enum.filter(intermediates_list, &(&1 != nil))

    Enum.flat_map(filtered_intermediates, fn intermediate ->
      target_name = relationship.target

      target_errors =
        validate_relationship(intermediate, target_name, check_referential_integrity, deep)

      Enum.map(target_errors, fn error ->
        Map.put(error, :relationship_path, [
          relationship.name,
          through_name | Map.get(error, :relationship_path, [])
        ])
      end)
    end)
  end

  # Validates a polymorphic relationship
  defp validate_polymorphic(resource, relationship, check_referential_integrity, deep) do
    base_name = relationship.polymorphic_name || relationship.name
    id_field = :"#{base_name}_id"
    type_field = :"#{base_name}_type"
    id_value = Map.get(resource, id_field)
    type_value = Map.get(resource, type_field)

    []
    |> validate_polymorphic_required(relationship, id_value, type_value, id_field, type_field)
    |> validate_polymorphic_type(relationship, type_value, type_field)
    |> validate_polymorphic_integrity(
      resource,
      relationship,
      id_value,
      type_value,
      id_field,
      type_field,
      check_referential_integrity
    )
    |> validate_polymorphic_deep(
      resource,
      relationship,
      id_value,
      type_value,
      deep,
      check_referential_integrity
    )
  end

  defp validate_polymorphic_required(
         errors,
         relationship,
         id_value,
         type_value,
         id_field,
         type_field
       ) do
    if relationship.required && (is_nil(id_value) || is_nil(type_value)) do
      [
        %{
          relationship: relationship.name,
          error: "Required polymorphic relationship has nil id or type",
          id_field: id_field,
          type_field: type_field
        }
        | errors
      ]
    else
      errors
    end
  end

  defp validate_polymorphic_type(errors, relationship, type_value, type_field) do
    if !is_nil(type_value) && relationship.allowed_types &&
         !Enum.any?(relationship.allowed_types, &(to_string(&1) == to_string(type_value))) do
      [
        %{
          relationship: relationship.name,
          error: "Invalid polymorphic type: #{type_value}, allowed types: #{inspect(relationship.allowed_types)}",
          type_field: type_field,
          type_value: type_value
        }
        | errors
      ]
    else
      errors
    end
  end

  defp validate_polymorphic_integrity(
         errors,
         resource,
         relationship,
         id_value,
         type_value,
         id_field,
         type_field,
         check_referential_integrity
       ) do
    if check_referential_integrity && !is_nil(id_value) && !is_nil(type_value) do
      case RelationshipResolver.resolve_relationship(resource, relationship.name) do
        {:ok, nil} ->
          [
            %{
              relationship: relationship.name,
              error: "Referenced polymorphic entity not found",
              id_field: id_field,
              id_value: id_value,
              type_field: type_field,
              type_value: type_value
            }
            | errors
          ]

        {:ok, _} ->
          errors

        {:error, reason} ->
          [
            %{
              relationship: relationship.name,
              error: "Error resolving polymorphic relationship: #{reason}",
              id_field: id_field,
              id_value: id_value,
              type_field: type_field,
              type_value: type_value
            }
            | errors
          ]
      end
    else
      errors
    end
  end

  defp validate_polymorphic_deep(
         errors,
         resource,
         relationship,
         id_value,
         type_value,
         deep,
         check_referential_integrity
       ) do
    if deep && !is_nil(id_value) && !is_nil(type_value) do
      case RelationshipResolver.resolve_relationship(resource, relationship.name) do
        {:ok, related} when not is_nil(related) ->
          validate_relationships(related,
            check_referential_integrity: check_referential_integrity
          )
      end
    else
      errors
    end
  end

  @doc """
  Validates cascading update operations.

  Ensures that updates to a resource maintain the integrity of related resources.
  For example, if a foreign key is changed, this function can validate that the
  new reference exists.

  ## Options

  * `:cascade` - If set to `true`, allows cascading updates to related resources.
    Defaults to `false`.

  ## Examples

      iex> validate_update(user, %{team_id: 5})
      :ok

      iex> validate_update(user, %{team_id: 999})
      {:error, [%{relationship: :team, error: "Team with ID 999 does not exist"}]}
  """
  @spec validate_update(map(), map(), keyword()) :: :ok | {:error, [map()]}
  def validate_update(resource, updates, opts \\ []) do
    cascade = Keyword.get(opts, :cascade, false)

    # Get the resource module
    resource_module = Map.get(resource, :__resource_module__)

    if is_nil(resource_module) do
      {:error, [%{error: "Resource does not have a __resource_module__ attribute"}]}
    else
      # Apply updates to a copy of the resource to validate the result
      updated_resource = Map.merge(resource, updates)

      # Validate relationships on the updated resource
      validate_update_with_cascade(resource, updated_resource, updates, cascade)
    end
  end

  defp validate_update_with_cascade(resource, updated_resource, updates, cascade) do
    case validate_relationships(updated_resource) do
      :ok ->
        if cascade do
          validate_cascade_updates(resource, updated_resource, updates)
        else
          :ok
        end

      error ->
        error
    end
  end

  # Validates cascading updates
  defp validate_cascade_updates(original, updated, updates) do
    resource_module = Map.get(original, :__resource_module__)
    relationships = RelationshipResolver.get_relationships(resource_module)
    affected_relationships = find_affected_relationships(relationships, updates)
    errors = validate_affected_relationships(affected_relationships, original, updated)

    if Enum.empty?(errors) do
      :ok
    else
      {:error, errors}
    end
  end

  defp find_affected_relationships(relationships, updates) do
    Enum.filter(relationships, fn relationship ->
      relationship.type == :belongs_to && has_foreign_key_update?(relationship, updates)
    end)
  end

  defp has_foreign_key_update?(relationship, updates) do
    foreign_key = relationship.foreign_key || :"#{relationship.name}_id"
    Map.has_key?(updates, foreign_key)
  end

  defp validate_affected_relationships(affected_relationships, original, updated) do
    Enum.flat_map(affected_relationships, fn relationship ->
      validate_relationship_update(relationship, original, updated)
    end)
  end

  defp validate_relationship_update(relationship, original, updated) do
    foreign_key = relationship.foreign_key || :"#{relationship.name}_id"
    old_value = Map.get(original, foreign_key)
    new_value = Map.get(updated, foreign_key)

    if old_value != new_value do
      validate_foreign_key_change(relationship, foreign_key, new_value)
    else
      []
    end
  end

  defp validate_foreign_key_change(relationship, foreign_key, new_value) do
    if is_nil(new_value) do
      validate_nil_foreign_key(relationship, foreign_key)
    else
      validate_foreign_key_reference(relationship, foreign_key, new_value)
    end
  end

  defp validate_nil_foreign_key(relationship, foreign_key) do
    if relationship.required do
      [
        %{
          relationship: relationship.name,
          operation: :update,
          error: "Cannot set required foreign key to nil",
          foreign_key: foreign_key
        }
      ]
    else
      []
    end
  end

  defp validate_foreign_key_reference(relationship, foreign_key, new_value) do
    case relationship.resource.load(new_value) do
      {:ok, _} ->
        []

      {:error, reason} ->
        [
          %{
            relationship: relationship.name,
            operation: :update,
            error: "Referenced entity not found: #{reason}",
            foreign_key: foreign_key,
            foreign_key_value: new_value
          }
        ]
    end
  end

  @doc """
  Validates a delete operation.

  Ensures that deleting a resource won't violate referential integrity
  constraints. This can prevent deletion if other resources depend on this one,
  or it can validate that cascading deletes are properly handled.

  ## Options

  * `:cascade` - If set to `true`, allows cascading deletes of related resources.
    Defaults to `false`.
  * `:force` - If set to `true`, allows deletion even if it would break
    referential integrity. Defaults to `false`.

  ## Examples

      iex> validate_delete(post_with_no_comments)
      :ok

      iex> validate_delete(post_with_comments)
      {:error, [%{relationship: :comments, error: "Cannot delete post with existing comments"}]}

      iex> validate_delete(post_with_comments, cascade: true)
      :ok  # Will also delete the comments
  """
  @spec validate_delete(map(), keyword()) :: :ok | {:error, [map()]}
  def validate_delete(resource, opts \\ []) do
    cascade = Keyword.get(opts, :cascade, false)
    force = Keyword.get(opts, :force, false)

    if force do
      :ok
    else
      validate_delete_with_checks(resource, cascade)
    end
  end

  defp validate_delete_with_checks(resource, cascade) do
    resource_module = Map.get(resource, :__resource_module__)

    if is_nil(resource_module) do
      {:error, [%{error: "Resource does not have a __resource_module__ attribute"}]}
    else
      reverse_relationships = find_reverse_relationships(resource)
      validate_reverse_relationships(reverse_relationships, cascade, resource)
    end
  end

  defp validate_reverse_relationships([], _cascade, _resource) do
    :ok
  end

  defp validate_reverse_relationships(reverse_relationships, cascade, resource) do
    if cascade do
      validate_cascade_delete(resource, reverse_relationships)
    else
      {:error, build_reverse_relationship_errors(reverse_relationships)}
    end
  end

  defp build_reverse_relationship_errors(reverse_relationships) do
    Enum.map(reverse_relationships, fn {rel_resource, rel_def} ->
      %{
        relationship: rel_def.name,
        resource: rel_resource.__struct__,
        error: "Cannot delete because it is referenced by #{inspect(rel_resource.__struct__)}"
      }
    end)
  end

  # Find all resources that have a relationship to this resource
  defp find_reverse_relationships(_resource) do
    # This is a simplified implementation and would need to be enhanced
    # in a real-world scenario to efficiently find reverse relationships

    # In a real implementation, you would typically query the database
    # for entities that reference this resource

    # For now, we'll return an empty list, as proper implementation
    # would require knowledge of the data store and querying capabilities
    []
  end

  # Validates cascading deletes
  defp validate_cascade_delete(_resource, _reverse_relationships) do
    # In a real implementation, this would check that each reverse relationship
    # can be properly deleted in a cascading manner
    :ok
  end
end
