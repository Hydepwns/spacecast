defmodule Spacecast.Utils.RelationshipDSL do
  @moduledoc """
  Private relationship DSL functions for the LiveViewResource system.

  These functions are used internally by the DSL macros but are not meant
  to be called directly by resource definitions.
  """

  @doc false
  def _relationship_belongs_to__(name, resource) do
    relationship_def = %{
      name: name,
      type: :belongs_to,
      resource: resource,
      foreign_key: nil,
      cardinality: :one
    }

    Module.put_attribute(__MODULE__, :resource_relationships, relationship_def)
  end

  @doc false
  def _relationship_belongs_to_with_opts__(name, resource, opts) do
    relationship_def = %{
      name: name,
      type: :belongs_to,
      resource: resource,
      foreign_key: Keyword.get(opts, :foreign_key, nil),
      cardinality: :one
    }

    Module.put_attribute(__MODULE__, :resource_relationships, relationship_def)
  end

  @doc false
  def _relationship_has_many__(name, resource) do
    relationship_def = %{
      name: name,
      type: :has_many,
      resource: resource,
      foreign_key: nil,
      cardinality: :many
    }

    Module.put_attribute(__MODULE__, :resource_relationships, relationship_def)
  end

  @doc false
  def _relationship_has_many_with_opts__(name, resource, opts) do
    relationship_def = %{
      name: name,
      type: :has_many,
      resource: resource,
      foreign_key: Keyword.get(opts, :foreign_key, nil),
      cardinality: :many
    }

    Module.put_attribute(__MODULE__, :resource_relationships, relationship_def)
  end

  @doc false
  def _relationship_has_many_through__(name, opts) do
    case opts do
      [through: [through_rel, target_rel]] ->
        relationship_def = %{
          name: name,
          type: :through,
          through: through_rel,
          target: target_rel,
          cardinality: :many,
          foreign_key: nil
        }

        Module.put_attribute(__MODULE__, :resource_relationships, relationship_def)

      _ ->
        raise ArgumentError, "has_many_through expects opts: [through: [rel1, rel2]]"
    end
  end

  @doc false
  def _relationship_has_many_through_with_opts__(name, opts, through_opts) do
    case opts do
      [through: [through_rel, target_rel]] ->
        relationship_def = %{
          name: name,
          type: :through,
          through: through_rel,
          target: target_rel,
          cardinality: :many,
          foreign_key: Keyword.get(through_opts, :foreign_key, nil)
        }

        Module.put_attribute(__MODULE__, :resource_relationships, relationship_def)

      _ ->
        raise ArgumentError, "has_many_through_with_opts expects opts: [through: [rel1, rel2]]"
    end
  end

  @doc false
  def _relationship_has_one__(name, resource) do
    relationship_def = %{
      name: name,
      type: :has_one,
      resource: resource,
      foreign_key: nil,
      cardinality: :one
    }

    Module.put_attribute(__MODULE__, :resource_relationships, relationship_def)
  end

  @doc false
  def _relationship_has_one_with_opts__(name, resource, opts) do
    relationship_def = %{
      name: name,
      type: :has_one,
      resource: resource,
      foreign_key: Keyword.get(opts, :foreign_key, nil),
      cardinality: :one
    }

    Module.put_attribute(__MODULE__, :resource_relationships, relationship_def)
  end

  @doc false
  def _relationship_has_one_through__(name, opts) do
    case opts do
      [through: [through_rel, target_rel]] ->
        relationship_def = %{
          name: name,
          type: :through,
          through: through_rel,
          target: target_rel,
          cardinality: :one,
          foreign_key: nil
        }

        Module.put_attribute(__MODULE__, :resource_relationships, relationship_def)

      _ ->
        raise ArgumentError, "has_one_through expects opts: [through: [rel1, rel2]]"
    end
  end

  @doc false
  def _relationship_has_one_through_with_opts__(name, opts, through_opts) do
    case opts do
      [through: [through_rel, target_rel]] ->
        relationship_def = %{
          name: name,
          type: :through,
          through: through_rel,
          target: target_rel,
          cardinality: :one,
          foreign_key: Keyword.get(through_opts, :foreign_key, nil)
        }

        Module.put_attribute(__MODULE__, :resource_relationships, relationship_def)

      _ ->
        raise ArgumentError, "has_one_through_with_opts expects opts: [through: [rel1, rel2]]"
    end
  end

  @doc false
  def _relationship_polymorphic__(name, opts) do
    case opts do
      [types: allowed_types] ->
        relationship_def = %{
          name: name,
          type: :polymorphic,
          polymorphic_name: name,
          allowed_types: allowed_types,
          cardinality: :one
        }

        Module.put_attribute(__MODULE__, :resource_relationships, relationship_def)

      _ ->
        raise ArgumentError, "polymorphic expects opts: [types: allowed_types]"
    end
  end

  @doc false
  def _relationship_polymorphic_with_opts__(name, opts, poly_opts) do
    case opts do
      [types: allowed_types] ->
        polymorphic_name = Keyword.get(poly_opts, :polymorphic_name, name)

        relationship_def = %{
          name: name,
          type: :polymorphic,
          polymorphic_name: polymorphic_name,
          allowed_types: allowed_types,
          cardinality: Keyword.get(poly_opts, :cardinality, :one)
        }

        Module.put_attribute(__MODULE__, :resource_relationships, relationship_def)

      _ ->
        raise ArgumentError, "polymorphic_with_opts expects opts: [types: allowed_types]"
    end
  end
end
