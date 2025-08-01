defmodule Spacecast.Utils.ResourceAssigns do
  @moduledoc """
  Implements an Ash-inspired resource architecture for LiveView socket assigns.

  This module provides a DSL for declaratively defining socket assigns as first-class resources
  with attributes, relationships, and validations.

  ## Features

  - Declarative assign specifications using DSL
  - Resource-oriented socket assigns
  - API-based access patterns for LiveView resources
  - Bidirectional integration with Ash resources (when available)

  ## Usage

  ```elixir
  defmodule MyAppWeb.UserLive do
    use SpacecastWeb.Resources.ResourceLive
    
    assigns_resource do
      attributes do
        attribute :user_id, :string, required: true
        attribute :username, :string, required: true
        attribute :role, {:one_of, ["admin", "user", "guest"]}, default: "user"
        
        # Nested attributes using map schema
        nested_attribute :settings, :map do
          attribute :theme, {:one_of, ["dark", "light", "system"]}, default: "system"
          attribute :notifications, :boolean, default: true
        end
        
        # List attributes with validation
        attribute :permissions, {:list, :string}, default: []
      end
      
      relationships do
        # Define relationships to other socket resources
        # (Implementation pending)
      end
      
      validations do
        # Custom validations beyond type validation
        # (Implementation pending)
      end
    end
    
    # Your LiveView implementation...
  end
  ```
  """

  @doc """
  Defines a resource for LiveView assigns.

  This macro is the entry point for the DSL, allowing developers to define
  attributes, relationships, and validations for LiveView socket assigns.
  """
  defmacro assigns_resource(do: block) do
    quote do
      Module.register_attribute(__MODULE__, :resource_attributes, accumulate: true)
      Module.register_attribute(__MODULE__, :resource_relationships, accumulate: true)
      Module.register_attribute(__MODULE__, :resource_validations, accumulate: true)

      import Spacecast.Utils.ResourceAssigns,
        only: [attributes: 1, relationships: 1, validations: 1]

      unquote(block)

      # Generate the required assigns and type specs based on the resource definition
      @before_compile Spacecast.Utils.ResourceAssigns
    end
  end

  @doc """
  Defines attributes for the socket assigns resource.
  """
  defmacro attributes(do: block) do
    quote do
      import Spacecast.Utils.ResourceAssigns,
        only: [attribute: 2, attribute: 3, nested_attribute: 4]

      unquote(block)
    end
  end

  @doc """
  Defines relationships for the socket assigns resource.
  """
  defmacro relationships(do: block) do
    quote do
      import Spacecast.Utils.ResourceAssigns, only: [relationship: 3, relationship: 4]
      unquote(block)
    end
  end

  @doc """
  Defines validations for the socket assigns resource.
  """
  defmacro validations(do: block) do
    quote do
      import Spacecast.Utils.ResourceAssigns, only: [validation: 2, validation: 3]
      unquote(block)
    end
  end

  @doc """
  Defines an attribute for the socket assigns resource.
  """
  defmacro attribute(name, type, opts \\ []) do
    processed_opts =
      if Keyword.has_key?(opts, :default) do
        default = Keyword.get(opts, :default)

        escaped_default =
          case default do
            %{} when map_size(default) == 0 -> Macro.escape(%{})
            m when is_map(m) or is_list(m) -> Macro.escape(m)
            _ -> default
          end

        Keyword.put(Keyword.delete(opts, :default), :default, escaped_default)
      else
        opts
      end

    # Escape the entire opts keyword list before injecting it into the quoted expression
    escaped_opts = Macro.escape(processed_opts)

    quote do
      @resource_attributes {unquote(name), unquote(type), unquote(escaped_opts)}
    end
  end

  @doc """
  Defines a nested attribute with a block for nested attributes.
  """
  defmacro nested_attribute(name, type, opts \\ [], do: block) when type == :map do
    # Compute nested attributes at macro expansion time
    nested_module = Module.concat(__CALLER__.module, "Nested#{:rand.uniform(1_000_000)}")

    defmodule nested_module do
      Module.register_attribute(__MODULE__, :nested_attributes, accumulate: true)
      _block_result = block
      def get_nested_attributes, do: @nested_attributes
    end

    nested_attrs = nested_module.get_nested_attributes()

    nested_schema =
      Enum.reduce(nested_attrs, %{}, fn {attr_name, attr_type, attr_opts}, acc ->
        Map.put(
          acc,
          attr_name,
          Spacecast.Utils.ResourceAssigns.build_type_spec(attr_type, attr_opts)
        )
      end)

    # Build nested defaults
    nested_defaults =
      Enum.reduce(nested_attrs, %{}, fn {attr_name, _attr_type, attr_opts}, acc ->
        if Keyword.has_key?(attr_opts, :default) do
          Map.put(acc, attr_name, Keyword.get(attr_opts, :default))
        else
          acc
        end
      end)

    # Process opts with proper escaping
    processed_opts =
      if Keyword.has_key?(opts, :default) do
        # If a default is explicitly provided, use it
        default = Keyword.get(opts, :default)
        Keyword.put(Keyword.delete(opts, :default), :default, default)
      else
        # If no default is provided, use the nested defaults
        Keyword.put(opts, :default, nested_defaults)
      end

    # Escape the nested_schema before injecting it into the quoted expression
    escaped_nested_schema = Macro.escape(nested_schema)
    escaped_processed_opts = Macro.escape(processed_opts)

    quote do
      @resource_attributes {unquote(name), unquote(escaped_nested_schema),
                            unquote(escaped_processed_opts)}
    end
  end

  @doc """
  Defines a relationship for the socket assigns resource.
  """
  defmacro relationship(name, type, target, opts \\ []) do
    quote do
      @resource_relationships {unquote(name), unquote(type), unquote(target), unquote(opts)}
    end
  end

  @doc """
  Defines a validation for the socket assigns resource.
  """
  defmacro validation(name, validation_fn, opts \\ []) do
    quote do
      @resource_validations {unquote(name), unquote(validation_fn), unquote(opts)}
    end
  end

  @doc """
  Builds a type specification based on attribute type and options.
  """
  def build_type_spec(type, opts) do
    # If the attribute is optional, wrap the type in an optional spec
    if Keyword.get(opts, :optional, false) do
      {:optional, type}
    else
      type
    end
  end

  defp deep_escape(value) when is_map(value) or is_list(value), do: Macro.escape(value)
  defp deep_escape(value), do: value

  @doc """
  Before compile hook to process resource definitions and generate required code.
  """
  defmacro __before_compile__(env) do
    # Collect all the resource definitions
    attributes = Module.get_attribute(env.module, :resource_attributes) || []
    relationships = Module.get_attribute(env.module, :resource_relationships) || []
    validations = Module.get_attribute(env.module, :resource_validations) || []

    # Build metadata
    required_assigns = build_required_assigns(attributes)
    type_specs = build_type_specs(attributes)
    default_values = build_default_values(attributes)
    escaped_default_values = deep_escape(default_values)

    # Generate functions
    apply_defaults_function = generate_apply_defaults_function(escaped_default_values)
    accessors = generate_accessors(attributes)
    init_function = generate_init_function(escaped_default_values)

    # Combine everything and generate the code
    quote do
      use SpacecastWeb.Resources.ResourceLive,
        required_assigns: unquote(required_assigns),
        type_specs: unquote(Macro.escape(type_specs))

      # Generate accessor functions
      unquote(accessors)

      # Generate initialization function
      unquote(init_function)

      # Generate apply defaults function
      unquote(apply_defaults_function)

      # Override do_mount to include default values
      def do_mount(params, session, socket) do
        socket = init_resource_assigns(socket)
        super(params, session, socket)
      end

      # Metadata about the resource
      def __resource_metadata__() do
        %{
          attributes: unquote(Macro.escape(attributes)),
          relationships: unquote(Macro.escape(relationships)),
          validations: unquote(Macro.escape(validations)),
          required_assigns: unquote(required_assigns),
          type_specs: unquote(Macro.escape(type_specs)),
          default_values: unquote(escaped_default_values)
        }
      end
    end
  end

  # Helper functions to reduce complexity
  defp build_required_assigns(attributes) do
    attributes
    |> Enum.filter(fn {_name, _type, opts} ->
      !Keyword.get(opts, :optional, false) && !Keyword.has_key?(opts, :default)
    end)
    |> Enum.map(fn {name, _type, _opts} -> name end)
  end

  defp build_type_specs(attributes) do
    attributes
    |> Enum.map(fn {name, type, opts} ->
      {name, build_type_spec(type, opts)}
    end)
    |> Enum.into(%{})
  end

  defp build_default_values(attributes) do
    attributes
    |> Enum.filter(fn {_name, _type, opts} -> Keyword.has_key?(opts, :default) end)
    |> Enum.map(fn {name, _type, opts} ->
      default = Keyword.get(opts, :default)
      # Ensure the default is a simple value that Phoenix.Component.assign can handle
      flattened_default =
        case default do
          %{} when map_size(default) == 0 -> %{}
          m when is_map(m) -> m
          l when is_list(l) -> l
          other -> other
        end

      {name, flattened_default}
    end)
    |> Enum.into(%{})
  end

  defp generate_apply_defaults_function(default_values) do
    quote do
      def __apply_resource_defaults__(socket) do
        defaults = unquote(Macro.escape(default_values))
        Phoenix.Component.assign(socket, defaults)
      end
    end
  end

  defp generate_accessors(attributes) do
    Enum.map(attributes, fn {name, _type, opts} ->
      quote do
        def unquote(name)(socket) do
          Spacecast.Utils.SocketValidator.get_assign(
            socket,
            unquote(name),
            unquote(Keyword.get(opts, :default))
          )
        end

        def unquote(:"put_#{name}")(socket, value) do
          Phoenix.Component.assign(socket, unquote(name), value)
        end
      end
    end)
  end

  defp generate_init_function(default_values) do
    quote do
      def init_resource_assigns(socket) do
        defaults = unquote(Macro.escape(default_values))
        Phoenix.Component.assign(socket, defaults)
      end
    end
  end
end
