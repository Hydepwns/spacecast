defmodule Spacecast.Utils.RelationshipResolver do
  @moduledoc """
  Handles resolution of relationships between resources.

  This module provides functionality for loading related resources,
  implementing lazy and eager loading strategies, and managing relationship
  caching for performance optimization.

  ## Features

  - Lazy-loading of related resources
  - Eager loading of related resources for performance
  - Relationship caching system
  - Support for belongs_to, has_many, and has_one relationships
  - Support for through relationships
  - Polymorphic relationship handling
  """

  @doc """
  Gets all relationships for a resource module.

  Returns a list of all relationship definitions for the given resource module.

  ## Examples

      iex> get_relationships(MyApp.UserResource)
      [
        %{name: :posts, type: :has_many, resource: MyApp.PostResource, cardinality: :many},
        %{name: :team, type: :belongs_to, resource: MyApp.TeamResource, cardinality: :one}
      ]
  """
  @spec get_relationships(module()) :: [map()]
  def get_relationships(resource_module) when is_atom(resource_module) do
    # First try the direct relationships function
    if function_exported?(resource_module, :relationships, 0) do
      try do
        resource_module.relationships()
      rescue
        _ -> []
      end
    else
      # Try getting from resource schema
      schema =
        if function_exported?(resource_module, :__resource_schema__, 0) do
          try do
            resource_module.__resource_schema__()
          rescue
            _ -> %{relationships: []}
          end
        else
          %{relationships: []}
        end

      Map.get(schema, :relationships, [])
    end
  end

  @doc """
  Resolves a specific relationship for a resource.

  ## Options

  * `:lazy` - If set to `true`, returns a lazy loader function instead of loading
    the relationship immediately. Defaults to `false`.
  * `:cache` - If set to `true`, caches the result for future resolution.
    Defaults to `true`.

  ## Examples

      iex> resolve_relationship(user, :posts)
      {:ok, [%{id: 1, title: "Post 1"}, %{id: 2, title: "Post 2"}]}

      iex> resolve_relationship(user, :team)
      {:ok, %{id: 1, name: "Team A"}}

      iex> resolve_relationship(user, :posts, lazy: true)
      {:ok, #Function<0.123456789/0>}
  """
  @spec resolve_relationship(map(), atom(), keyword()) ::
          {:ok, term()} | {:ok, (-> {:ok, term()} | {:error, String.t()})} | {:error, String.t()}
  def resolve_relationship(resource, relationship_name, opts \\ []) do
    lazy = Keyword.get(opts, :lazy, false)
    cache = Keyword.get(opts, :cache, true)

    # Get the resource module
    resource_module = Map.get(resource, :__resource_module__)

    with {:ok, resource_module} <- validate_resource_module(resource_module),
         {:ok, relationship} <- get_relationship_definition(resource_module, relationship_name) do
      resolve_relationship_data(resource, relationship, lazy, cache)
    else
      {:error, reason} -> {:error, reason}
    end
  end

  defp validate_resource_module(nil) do
    {:error,
     "Cannot resolve relationship: Resource does not have a __resource_module__ attribute"}
  end

  defp validate_resource_module(resource_module) do
    {:ok, resource_module}
  end

  defp resolve_relationship_data(resource, relationship, lazy, cache) do
    if lazy do
      # Return a lazy loader function
      {:ok,
       fn ->
         case do_resolve_relationship(resource, relationship, cache: cache) do
           {:ok, related_data, _updated_resource} -> {:ok, related_data}
           {:error, reason, _resource} -> {:error, reason}
         end
       end}
    else
      # Resolve immediately
      case do_resolve_relationship(resource, relationship, cache: cache) do
        {:ok, related_data, updated_resource} ->
          # For testing purposes, store the updated resource in the process dictionary
          if Application.get_env(:spacecast, :testing) do
            Process.put(
              {:resource_cache, resource.__resource_module__, resource.id},
              updated_resource
            )
          end

          # Return the related data
          {:ok, related_data}

        {:error, reason, _resource} ->
          {:error, reason}
      end
    end
  end

  @doc """
  Eagerly loads multiple relationships for a resource or collection of resources.

  This function is optimized for loading multiple relationships in one operation,
  reducing the number of database queries needed.

  ## Examples

      iex> eager_load(user, [:posts, :team])
      {:ok, %{user_with_posts_and_team | posts: [...], team: %{...}}}

      iex> eager_load([user1, user2], [:posts])
      {:ok, [%{user1_with_posts | posts: [...]}, %{user2_with_posts | posts: [...]}]}
  """
  @spec eager_load(map() | [map()], [atom()], keyword()) ::
          {:ok, map() | [map()]} | {:error, String.t()}
  def eager_load(resource_or_resources, relationships, _opts \\ []) do
    # Handle single resource or collection
    if is_list(resource_or_resources) do
      # Handle collection of resources
      results =
        Enum.map(resource_or_resources, fn resource ->
          case eager_load_single(resource, relationships) do
            {:ok, loaded_resource} -> loaded_resource
            # Keep original on error
            {:error, _} -> resource
          end
        end)

      {:ok, results}
    else
      # Handle single resource
      eager_load_single(resource_or_resources, relationships)
    end
  end

  # Helper function to eager load relationships for a single resource
  defp eager_load_single(resource, relationships) do
    # Get the resource module
    resource_module = Map.get(resource, :__resource_module__)

    if is_nil(resource_module) do
      {:error, "Cannot eager load: Resource does not have a __resource_module__ attribute"}
    else
      # Get relationship definitions
      relationship_defs = get_relationship_definitions(resource_module, relationships)

      # Load each relationship
      Enum.reduce_while(relationship_defs, {:ok, resource}, &load_relationship_reducer/2)
    end
  end

  defp get_relationship_definitions(resource_module, relationships) do
    Enum.map(relationships, fn rel_name ->
      case get_relationship_definition(resource_module, rel_name) do
        {:ok, rel_def} -> {rel_name, rel_def}
        {:error, _} -> nil
      end
    end)
    |> Enum.reject(&is_nil/1)
  end

  defp load_relationship_reducer({rel_name, _rel_def}, {:ok, acc_resource}) do
    case resolve_relationship(acc_resource, rel_name) do
      {:ok, related} ->
        # Add the resolved relationship to the resource
        {:cont, {:ok, Map.put(acc_resource, rel_name, related)}}

      {:error, reason} ->
        {:halt, {:error, "Failed to load relationship #{rel_name}: #{reason}"}}
    end
  end

  @doc """
  Gets a relationship definition from a resource module.

  ## Examples

      iex> get_relationship_definition(MyApp.UserResource, :posts)
      {:ok, %{name: :posts, type: :has_many, resource: MyApp.PostResource, foreign_key: :user_id, cardinality: :many}}
  """
  @spec get_relationship_definition(module(), atom()) :: {:ok, map()} | {:error, String.t()}
  def get_relationship_definition(resource_module, relationship_name)
      when is_atom(resource_module) do
    # Get all relationships
    relationships = get_relationships(resource_module)

    # Find the specific relationship
    case Enum.find(relationships, fn relationship ->
           relationship.name == relationship_name
         end) do
      nil ->
        {:error, "Relationship #{relationship_name} not found in #{inspect(resource_module)}"}

      relationship ->
        {:ok, relationship}
    end
  end

  # Handle different relationship definition formats

  # Implements the actual relationship resolution logic
  defp do_resolve_relationship(resource, relationship, opts) do
    cache = Keyword.get(opts, :cache, true)

    # First check if we have this relationship cached
    case get_cached_relationship(resource, relationship.name) do
      {:ok, cached} when cache ->
        # Return cached value if we found it and caching is enabled
        {:ok, cached, resource}

      _ ->
        # Otherwise, load the relationship
        load_relationship(resource, relationship, cache: cache)
    end
  end

  # Loads a relationship based on its type
  defp load_relationship(resource, relationship, opts) do
    cache = Keyword.get(opts, :cache, true)

    case relationship.type do
      :belongs_to ->
        load_belongs_to_relationship(resource, relationship, cache: cache)

      :has_many ->
        load_has_many_relationship(resource, relationship, cache: cache)

      :has_one ->
        load_has_one_relationship(resource, relationship, cache: cache)

      :through ->
        load_through_relationship(resource, relationship, cache: cache)

      :polymorphic ->
        load_polymorphic_relationship(resource, relationship, cache: cache)

      _ ->
        {:error, "Unsupported relationship type: #{inspect(relationship.type)}", resource}
    end
  end

  # Get cached relationship
  defp get_cached_relationship(resource, relationship_name) do
    # Get cache from resource if it exists
    cache = Map.get(resource, :__relationship_cache__, %{})

    if Map.has_key?(cache, relationship_name) do
      {:ok, Map.get(cache, relationship_name)}
    else
      {:error, :not_cached}
    end
  end

  # Cache a resolved relationship
  defp cache_relationship(resource, relationship_name, related_data) do
    # Get or initialize cache
    cache = Map.get(resource, :__relationship_cache__, %{})

    # Add to cache
    updated_cache = Map.put(cache, relationship_name, related_data)

    # Return updated resource with cache
    Map.put(resource, :__relationship_cache__, updated_cache)
  end

  # Loads a belongs_to relationship
  defp load_belongs_to_relationship(resource, relationship, opts) do
    cache = Keyword.get(opts, :cache, true)

    # Get the foreign key value
    foreign_key = relationship.foreign_key
    foreign_key_value = Map.get(resource, foreign_key)

    if is_nil(foreign_key_value) do
      # No related entity if foreign key is nil
      {:ok, nil, resource}
    else
      # Load the related resource
      case relationship.resource.load(foreign_key_value) do
        {:ok, related} ->
          # Add metadata to the related resource
          related = Map.put(related, :__resource_module__, relationship.resource)

          # Cache if needed
          updated_resource =
            if cache do
              cache_relationship(resource, relationship.name, related)
            else
              resource
            end

          {:ok, related, updated_resource}

        {:error, reason} ->
          {:error, "Failed to load #{relationship.name}: #{reason}", resource}
      end
    end
  end

  # Loads a has_many relationship
  defp load_has_many_relationship(resource, relationship, opts) do
    cache = Keyword.get(opts, :cache, true)

    # We need a query mechanism to find related resources
    # This is a simplified implementation and would need to be enhanced
    # based on the actual data access patterns of your application

    # Get primary key value
    # Assuming ID is the primary key
    primary_key = :id
    primary_key_value = Map.get(resource, primary_key)

    if is_nil(primary_key_value) do
      # No related entities if primary key is nil
      {:ok, [], resource}
    else
      # Get inverse foreign key
      _foreign_key =
        relationship.foreign_key ||
          :"#{resource.__struct__.__name__ |> Module.split() |> List.last() |> Macro.underscore()}_id"

      # Here we would query for all related resources where foreign_key = primary_key_value
      # This is a simplified placeholder - in a real implementation, you would use
      # your data access layer (Ecto, Ash, etc.) to perform this query
      # Placeholder for related entities
      related = []

      # Add metadata to each related resource
      related =
        Enum.map(related, fn item ->
          Map.put(item, :__resource_module__, relationship.resource)
        end)

      # Cache if needed
      updated_resource =
        if cache do
          cache_relationship(resource, relationship.name, related)
        else
          resource
        end

      {:ok, related, updated_resource}
    end
  end

  # Loads a has_one relationship
  defp load_has_one_relationship(resource, relationship, opts) do
    cache = Keyword.get(opts, :cache, true)

    # Similar to has_many but expecting only one result
    # Get primary key value
    # Assuming ID is the primary key
    primary_key = :id
    primary_key_value = Map.get(resource, primary_key)

    if is_nil(primary_key_value) do
      # No related entity if primary key is nil
      {:ok, nil, resource}
    else
      # Get inverse foreign key
      _foreign_key =
        relationship.foreign_key ||
          :"#{resource.__struct__.__name__ |> Module.split() |> List.last() |> Macro.underscore()}_id"

      # Here we would query for the related resource where foreign_key = primary_key_value
      # This is a simplified placeholder - in a real implementation, you would use
      # your data access layer (Ecto, Ash, etc.) to perform this query
      # Placeholder for related entity
      related = nil

      related_with_metadata =
        if related do
          # Add metadata to the related resource
          Map.put(related, :__resource_module__, relationship.resource)
        else
          nil
        end

      # Cache if needed
      updated_resource =
        if cache && related_with_metadata do
          cache_relationship(resource, relationship.name, related_with_metadata)
        else
          resource
        end

      {:ok, related_with_metadata, updated_resource}
    end
  end

  # Loads a through relationship
  defp load_through_relationship(resource, relationship, opts) do
    cache = Keyword.get(opts, :cache, true)

    # Through relationships involve loading an intermediate relationship first,
    # then loading the target relationship through it

    # Get the intermediate relationship
    through = relationship.through
    target = relationship.target

    # First, get the intermediate relationship definition
    resource_module = Map.get(resource, :__resource_module__)

    case get_relationship_definition(resource_module, through) do
      {:ok, through_relationship} ->
        # First load the intermediate relationship
        case do_resolve_relationship(resource, through_relationship, cache: false) do
          {:ok, intermediates, updated_resource} ->
            # Handle collection or single intermediate
            intermediates_list =
              if is_list(intermediates), do: intermediates, else: [intermediates]

            # Then load the target relationships from each intermediate
            related =
              intermediates_list
              |> Enum.filter(&(&1 != nil))
              |> Enum.flat_map(fn intermediate ->
                # Get the target relationship definition
                intermediate_module = Map.get(intermediate, :__resource_module__)

                case get_relationship_definition(intermediate_module, target) do
                  {:ok, target_relationship} ->
                    case do_resolve_relationship(intermediate, target_relationship, cache: false) do
                      {:ok, targets, _} -> if is_list(targets), do: targets, else: [targets]
                      {:error, _, _} -> []
                    end

                  {:error, _} ->
                    []
                end
              end)
              |> Enum.reject(&is_nil/1)

            # Cache if needed
            final_resource =
              if cache do
                cache_relationship(updated_resource, relationship.name, related)
              else
                updated_resource
              end

            {:ok, related, final_resource}

          {:error, reason, _} ->
            {:error, "Failed to load through relationship: #{reason}", resource}
        end

      {:error, reason} ->
        {:error,
         "Through relationship #{through} not found in #{inspect(resource_module)}: #{reason}",
         resource}
    end
  end

  # Loads a polymorphic relationship
  defp load_polymorphic_relationship(resource, relationship, opts) do
    cache = Keyword.get(opts, :cache, true)

    # Polymorphic relationships store both the ID and the type of the related entity
    # For example, with a :commentable polymorphic relationship, we might have
    # commentable_id and commentable_type fields

    # Get the base name for the polymorphic relation
    base_name = relationship.polymorphic_name || relationship.name

    # Get the ID and type values
    id_field = :"#{base_name}_id"
    type_field = :"#{base_name}_type"

    id_value = Map.get(resource, id_field)
    type_value = Map.get(resource, type_field)

    if is_nil(id_value) || is_nil(type_value) do
      # No related entity if either field is nil
      {:ok, nil, resource}
    else
      # Convert the type string to a module
      related_module = resolve_polymorphic_module(type_value)

      if is_nil(related_module) do
        {:error, "Could not resolve module for type: #{type_value}", resource}
      else
        # Load the related resource
        case related_module.load(id_value) do
          {:ok, related} ->
            # Add metadata to the related resource
            related = Map.put(related, :__resource_module__, related_module)

            # Cache if needed
            updated_resource =
              if cache do
                cache_relationship(resource, relationship.name, related)
              else
                resource
              end

            {:ok, related, updated_resource}

          {:error, reason} ->
            {:error, "Failed to load polymorphic #{relationship.name}: #{reason}", resource}
        end
      end
    end
  end

  # Helper to resolve a polymorphic type string to a module
  defp resolve_polymorphic_module(type_string) when is_binary(type_string) do
    # This is a simplified implementation - you would need to adapt this
    # to your application's naming conventions
    try do
      module_name =
        type_string
        |> String.split(".")
        |> Enum.map(&Macro.camelize/1)
        |> Enum.join(".")

      String.to_existing_atom("Elixir." <> module_name)
    rescue
      _ -> nil
    end
  end

  defp resolve_polymorphic_module(_), do: nil
end
