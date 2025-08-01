defmodule Spacecast.Utils.ResourceDSL do
  @moduledoc """
  Provides a DSL for defining resources in LiveView applications.
  """

  defmacro __using__(_opts) do
    quote do
      import Spacecast.Utils.ResourceDSL

      Module.register_attribute(__MODULE__, :attributes, accumulate: true)
      Module.register_attribute(__MODULE__, :validations, accumulate: true)
      Module.register_attribute(__MODULE__, :relationships, accumulate: true)

      # Only define struct if not already defined
      if !Module.defines?(__MODULE__, {:__struct__, 0}) do
        @struct_fields Module.get_attribute(__MODULE__, :attributes)
                       |> Enum.map(fn {name, _} -> name end)
                       |> Enum.concat([:__resource_module__])
                       |> Enum.uniq()
        defstruct @struct_fields
      end

      @before_compile Spacecast.Utils.ResourceDSL
    end
  end

  defmacro __before_compile__(_env) do
    quote do
      def attributes, do: @attributes |> Enum.into(%{})
      def validations, do: @validations
      def relationships, do: @relationships
    end
  end

  # Attribute DSL

  defmacro attribute(name, type, opts \\ []) do
    quote bind_quoted: [name: name, type: type, opts: opts] do
      attr_def = %{
        name: name,
        type: type,
        required: Keyword.get(opts, :required, false),
        default: Keyword.get(opts, :default, nil),
        format: Keyword.get(opts, :format, nil)
      }

      Module.put_attribute(__MODULE__, :attributes, {name, attr_def})
    end
  end

  # Validation DSL

  defmacro validate(name, validator_fn) do
    quote bind_quoted: [name: name, validator_fn: Macro.escape(validator_fn)] do
      Module.put_attribute(__MODULE__, :validations, {name, validator_fn})
    end
  end

  # Relationship DSL

  defmacro has_many(name, resource, opts \\ []) do
    quote bind_quoted: [name: name, resource: resource, opts: opts] do
      relationship_def = %{
        name: name,
        type: :has_many,
        resource: resource,
        foreign_key: Keyword.get(opts, :foreign_key, nil),
        cardinality: :many
      }

      Module.put_attribute(__MODULE__, :relationships, relationship_def)
    end
  end

  defmacro belongs_to(name, resource, opts \\ []) do
    quote bind_quoted: [name: name, resource: resource, opts: opts] do
      relationship_def = %{
        name: name,
        type: :belongs_to,
        resource: resource,
        foreign_key: Keyword.get(opts, :foreign_key, nil),
        cardinality: :one
      }

      Module.put_attribute(__MODULE__, :relationships, relationship_def)
    end
  end

  defmacro has_one(name, resource, opts \\ []) do
    quote bind_quoted: [name: name, resource: resource, opts: opts] do
      relationship_def = %{
        name: name,
        type: :has_one,
        resource: resource,
        foreign_key: Keyword.get(opts, :foreign_key, nil),
        cardinality: :one
      }

      Module.put_attribute(__MODULE__, :relationships, relationship_def)
    end
  end

  defmacro has_many_through(name, opts, through_opts \\ []) do
    quote bind_quoted: [name: name, opts: opts, through_opts: through_opts] do
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

          Module.put_attribute(__MODULE__, :relationships, relationship_def)

        _ ->
          raise ArgumentError, "has_many_through expects opts: [through: [rel1, rel2]]"
      end
    end
  end

  defmacro has_one_through(name, opts, through_opts \\ []) do
    quote bind_quoted: [name: name, opts: opts, through_opts: through_opts] do
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

          Module.put_attribute(__MODULE__, :relationships, relationship_def)

        _ ->
          raise ArgumentError, "has_one_through expects opts: [through: [rel1, rel2]]"
      end
    end
  end

  defmacro polymorphic(name, opts, poly_opts \\ []) do
    quote bind_quoted: [name: name, opts: opts, poly_opts: poly_opts] do
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

          Module.put_attribute(__MODULE__, :relationships, relationship_def)

        _ ->
          raise ArgumentError, "polymorphic expects opts: [types: allowed_types]"
      end
    end
  end
end
