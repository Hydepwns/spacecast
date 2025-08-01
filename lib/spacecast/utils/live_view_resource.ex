defmodule Spacecast.Utils.LiveViewResource do
  @compile :nowarn_unused_functions
  @moduledoc """
  Defines a behavior for resource-oriented LiveViews, inspired by Ash framework's resource patterns.

  LiveViewResource provides a structure for declaring socket assigns as first-class resources with
  attributes, validations, and relationships. This allows for more declarative LiveView development
  and consistent validation throughout the application.

  ## Usage

  ```elixir
  defmodule MyApp.UserResource do
    use Spacecast.Utils.LiveViewResource
    
    attributes do
      attribute :id, :string, required: true
      attribute :name, :string, required: true
      attribute :email, :string, format: ~r/@/
      attribute :role, {:one_of, ["admin", "editor", "viewer"]}, default: "viewer"
      attribute :settings, :map do
        attribute :notifications, :boolean, default: true
        attribute :theme, {:one_of, ["light", "dark", "system"]}, default: "system"
      end
    end
    
    relationships do
      has_many :posts, MyApp.PostResource
      belongs_to :team, MyApp.TeamResource
      has_many :team_members, through: [:team, :members]
      polymorphic :commentable, types: [MyApp.PostResource, MyApp.ArticleResource]
    end
    
    validations do
      validate :email_must_be_valid, fn resource ->
        # Custom validation logic
        if String.contains?(resource.email, "@") do
          :ok
        else
          {:error, "Email must contain @"}
        end
      end
      
      # Define a validation that depends on another validation
      validate :role_permissions_valid, fn resource ->
        if resource.role == "admin" && Enum.empty?(resource.permissions) do
          {:error, "Admin users must have at least one permission"}
        else
          :ok
        end
      end
      
      # Define that this validation depends on the email_must_be_valid validation
      validation_depends_on :role_permissions_valid, [:email_must_be_valid]
      
      # Define a validation that depends on a related resource validation
      validate_related :team, :max_members_not_exceeded, fn team ->
        if team.max_members && length(team.members) > team.max_members do
          {:error, "Team exceeds maximum member count"}
        else
          :ok
        end
      end
    end
  end
  ```

  ### Using with Data Source Adapters

  You can also define resources that are backed by a data source like an Ecto schema:

  ```elixir
  defmodule MyApp.UserResource do
    use Spacecast.Utils.LiveViewResource
    
    # Use the EctoAdapter with the User schema
    adapter Spacecast.Utils.EctoAdapter, schema: MyApp.User
  end
  ```
  """

  @doc """
  Defines the behavior required for a LiveViewResource.
  """
  @callback attributes() :: Macro.t()
  @callback relationships() :: Macro.t()
  @callback validations() :: Macro.t()

  @doc """
  Returns the schema for the resource, including all attributes, types, and validations.
  """
  @callback __resource_schema__() :: map()

  defmacro __using__(_opts) do
    quote do
      unquote(__using_impl__())
    end
  end

  defp __using_impl__ do
    quote do
      unquote(__behaviour_and_imports__())
      unquote(__module_attributes__())
      unquote(__before_compile__())
      unquote(__default_functions__())
      unquote(__change_tracking_functions__())
      unquote(__transformation_functions__())
      unquote(__validation_functions__())
      unquote(__resource_functions__())
      unquote(__adapter_functions__())
      unquote(__relationship_functions__())
    end
  end

  defp __behaviour_and_imports__ do
    quote do
      @behaviour Spacecast.Utils.LiveViewResource
      import Spacecast.Utils.LiveViewResource
    end
  end

  defp __module_attributes__ do
    quote do
      Module.register_attribute(__MODULE__, :resource_attributes, accumulate: true)
      Module.register_attribute(__MODULE__, :resource_relationships, accumulate: true)
      Module.register_attribute(__MODULE__, :validations, accumulate: true)
      Module.register_attribute(__MODULE__, :validation_dependencies, accumulate: true)
      Module.register_attribute(__MODULE__, :relationship_dependencies, accumulate: true)
      Module.register_attribute(__MODULE__, :transformations, accumulate: true)
      Module.register_attribute(__MODULE__, :adapter_info, accumulate: true)
    end
  end

  defp __before_compile__ do
    quote do
      @before_compile Spacecast.Utils.LiveViewResource
    end
  end

  defp __default_functions__ do
    quote do
      def attributes, do: []
      def relationships, do: []
      def validations, do: []
      defoverridable attributes: 0, relationships: 0, validations: 0
    end
  end

  defp __change_tracking_functions__ do
    quote do
      def track_change(resource, changes, metadata \\ %{}) do
        Spacecast.Utils.ChangeTracker.track_change(
          ensure_resource_module(resource),
          changes,
          metadata
        )
      end

      def get_history(resource, opts \\ []) do
        Spacecast.Utils.ChangeTracker.get_history(
          ensure_resource_module(resource),
          opts
        )
      end

      def get_version(resource, version) do
        Spacecast.Utils.ChangeTracker.get_version(
          ensure_resource_module(resource),
          version
        )
      end

      def diff_versions(resource, opts \\ []) do
        Spacecast.Utils.ChangeTracker.diff(
          ensure_resource_module(resource),
          opts
        )
      end

      def check_concurrent_update(resource, expected_version, changes) do
        Spacecast.Utils.ChangeTracker.check_concurrent_update(
          ensure_resource_module(resource),
          expected_version,
          changes
        )
      end

      def serialize_history(resource, opts \\ []) do
        Spacecast.Utils.ChangeTracker.serialize_history(
          ensure_resource_module(resource),
          opts
        )
      end

      def deserialize_history(resource, serialized, opts \\ []) do
        Spacecast.Utils.ChangeTracker.deserialize_history(
          ensure_resource_module(resource),
          serialized,
          opts
        )
      end
    end
  end

  defp __transformation_functions__ do
    quote do
      def register_transformation(module, opts \\ []) do
        transformation_def = %{
          module: module,
          opts: opts
        }

        Module.put_attribute(__MODULE__, :transformations, transformation_def)
      end

      def default_transformation_pipeline do
        transformations = @transformations
        Spacecast.Utils.LiveViewResource.build_transformation_pipeline(transformations)
      end
    end
  end

  defp __validation_functions__ do
    quote do
      def validate_resource(resource, context \\ %{}) do
        Spacecast.Utils.ValidationEngine.validate(
          ensure_resource_module(resource),
          resource,
          context
        )
      end

      def validate_attribute(resource, attribute_name, value, context \\ %{}) do
        Spacecast.Utils.ValidationEngine.validate_attribute(
          ensure_resource_module(resource),
          resource,
          attribute_name,
          value,
          context
        )
      end
    end
  end

  defp __resource_functions__ do
    quote do
      def __extract_attributes__ do
        @resource_attributes
      end

      def __extract_relationships__ do
        @resource_relationships
      end

      def __extract_validations__ do
        @validations
      end

      def ensure_resource_module(resource) when is_map(resource) do
        Map.get(resource, :__resource_module__, __MODULE__)
      end

      def ensure_resource_module(module) when is_atom(module) do
        module
      end
    end
  end

  defp __adapter_functions__ do
    quote do
      def adapter(module, opts \\ []) do
        adapter_info = [module: module] ++ opts
        Module.put_attribute(__MODULE__, :adapter_info, adapter_info)
      end
    end
  end

  defmacro adapter(module, opts \\ []) do
    quote do
      adapter_info = [module: unquote(module)] ++ unquote(opts)
      Module.put_attribute(__MODULE__, :adapter_info, adapter_info)
    end
  end

  defp __relationship_functions__ do
    quote do
      # This function provides the relationship DSL functions to the using module
      # The actual implementation is in __relationship_functions_impl/0
    end
  end

  defmacro relationships(do: block) do
    quote do
      def relationships do
        unquote(__relationship_functions_impl__())
        unquote(block)
        @resource_relationships
      end
    end
  end

  defp __relationship_functions_impl__ do
    quote do
      alias Spacecast.Utils.RelationshipDSL

      belongs_to = &RelationshipDSL._relationship_belongs_to__/2
      belongs_to_with_opts = &RelationshipDSL._relationship_belongs_to_with_opts__/3
      has_many = &RelationshipDSL._relationship_has_many__/2
      has_many_with_opts = &RelationshipDSL._relationship_has_many_with_opts__/3
      has_many_through = &RelationshipDSL._relationship_has_many_through__/2
      has_many_through_with_opts = &RelationshipDSL._relationship_has_many_through_with_opts__/3
      has_one = &RelationshipDSL._relationship_has_one__/2
      has_one_with_opts = &RelationshipDSL._relationship_has_one_with_opts__/3
      has_one_through = &RelationshipDSL._relationship_has_one_through__/2
      has_one_through_with_opts = &RelationshipDSL._relationship_has_one_through_with_opts__/3
      polymorphic = &RelationshipDSL._relationship_polymorphic__/2
      polymorphic_with_opts = &RelationshipDSL._relationship_polymorphic_with_opts__/3
    end
  end

  @doc """
  Before compile callback to process resource definitions and generate required code.
  """
  defmacro __before_compile__(env) do
    # Collect all the resource definitions
    attributes = Module.get_attribute(env.module, :resource_attributes) || []
    relationships = Module.get_attribute(env.module, :resource_relationships) || []
    validations = Module.get_attribute(env.module, :validations) || []
    validation_dependencies = Module.get_attribute(env.module, :validation_dependencies) || []
    relationship_dependencies = Module.get_attribute(env.module, :relationship_dependencies) || []
    transformations = Module.get_attribute(env.module, :transformations) || []
    adapter_info = Module.get_attribute(env.module, :adapter_info)

    type_specs =
      attributes
      |> Enum.map(fn {name, type, _opts} -> {name, type} end)
      |> Map.new()

    quote do
      @impl Spacecast.Utils.LiveViewResource
      def __resource_schema__ do
        %{
          attributes: unquote(Macro.escape(attributes)),
          relationships: unquote(Macro.escape(relationships)),
          validations: unquote(Macro.escape(validations)),
          validation_dependencies: unquote(Macro.escape(validation_dependencies)),
          relationship_dependencies: unquote(Macro.escape(relationship_dependencies)),
          transformations: unquote(Macro.escape(transformations)),
          adapter_info: unquote(Macro.escape(adapter_info))
        }
      end

      def __type_specs__ do
        unquote(Macro.escape(type_specs))
      end
    end
  end

  @doc """
  DSL for defining validations in a resource.
  """
  defmacro validations(do: block) do
    quote do
      @impl true
      def validations do
        import Spacecast.Utils.LiveViewResource,
          only: [
            validate: 2,
            validation_depends_on: 2,
            validate_related: 3
          ]

        # Execute the block to define validations
        unquote(block)

        # Return the accumulated validations from the module attribute
        @validations
      end
    end
  end

  @doc """
  DSL for defining a custom validation.
  """
  defmacro validate(name, validation_fn) do
    quote bind_quoted: [name: name, validation_fn: Macro.escape(validation_fn)] do
      validation_def = %{
        name: name,
        validation_fn: validation_fn
      }

      # Accumulate the validation at compile time
      Module.put_attribute(__MODULE__, :validations, validation_def)
    end
  end

  @doc """
  DSL for defining validation dependencies.

  This macro allows you to specify that a validation depends on other validations,
  either in the same resource or in related resources. The dependency information
  is used to determine the order in which validations should be executed.

  ## Example

  ```elixir
  # Simple dependencies within the same resource
  validation_depends_on :role_permissions_valid, [:email_must_be_valid]

  # Dependencies on validations in related resources
  validation_depends_on :team_role_valid, [{TeamResource, :role_valid}]
  ```
  """
  defmacro validation_depends_on(rule_name, dependencies) do
    quote bind_quoted: [rule_name: rule_name, dependencies: dependencies] do
      # Accumulate the validation dependency at compile time
      Module.put_attribute(__MODULE__, :validation_dependencies, {rule_name, dependencies})
    end
  end

  @doc """
  DSL for defining a validation on a related resource.

  This macro allows you to specify a validation that should be applied to a 
  related resource. The validation will only be executed if the relationship
  exists.

  ## Example

  ```elixir
  # Define a validation for a related resource
  validate_related :team, :team_name_valid, fn team ->
    if String.length(team.name) > 3 do
      :ok
    else
      {:error, "Team name must be at least 3 characters"}
    end
  end
  ```
  """
  defmacro validate_related(relationship_name, rule_name, rule_fn) do
    quote bind_quoted: [
            relationship_name: relationship_name,
            rule_name: rule_name,
            rule_fn: Macro.escape(rule_fn)
          ] do
      # Define the validation function that will be called on this resource
      validate(rule_name, fn resource, context ->
        # Check if the relationship exists
        case Spacecast.Utils.RelationshipResolver.resolve_relationship(
               resource,
               relationship_name
             ) do
          {:ok, nil} ->
            # Relationship doesn't exist, validation passes
            :ok

          {:ok, related} ->
            # Relationship exists, apply the validation function
            rule_fn.(related)

          {:error, _reason} ->
            # Error resolving relationship, validation passes
            :ok
        end
      end)

      # Register the relationship validation for dependency resolution
      Module.put_attribute(
        __MODULE__,
        :relationship_dependencies,
        {relationship_name, rule_name, []}
      )
    end
  end

  @doc """
  DSL for registering a transformation module with this resource.

  This macro allows you to specify a transformation that should be
  included in the default transformation pipeline for this resource.

  ## Example

  ```elixir
  # Register a transformation
  transformation MyApp.Transformations.NormalizeEmail

  # Register with options
  transformation MyApp.Transformations.NormalizeEmail,
    hook: :pre_validation,
    condition: fn resource, _context -> resource.email != nil end
  ```
  """
  defmacro transformation(module, opts \\ []) do
    quote bind_quoted: [module: module, opts: opts] do
      register_transformation(module, opts)
    end
  end

  @doc """
  Builds a transformation pipeline based on the registered transformations.

  This function is used by the default_transformation_pipeline/0 function
  to create a pipeline that includes all registered transformations.
  """
  def build_transformation_pipeline(transformations) do
    pipeline = Spacecast.Utils.TransformationPipeline.new()

    Enum.reduce(transformations, pipeline, fn transformation, acc ->
      Spacecast.Utils.TransformationPipeline.add(
        acc,
        transformation.module,
        transformation.opts
      )
    end)
  end

  @doc """
  Generates a type specification map from a resource definition.
  """
  def generate_type_specs(resource_module) when is_atom(resource_module) do
    schema = resource_module.__resource_schema__()
    adapter_info = Map.get(schema, :adapter_info)

    if adapter_info do
      # Use the adapter to generate type specs
      adapter_module = Keyword.get(adapter_info, :module)
      adapter_schema = Keyword.get(adapter_info, :schema)

      if function_exported?(adapter_module, :generate_type_specs, 1) do
        adapter_module.generate_type_specs(adapter_schema)
      else
        # Fall back to attribute-based type specs
        generate_type_specs_from_attributes(resource_module.__extract_attributes__())
      end
    else
      # Use attribute-based type specs
      generate_type_specs_from_attributes(schema.attributes)
    end
  end

  # Generate type specs from attribute definitions
  defp generate_type_specs_from_attributes(attributes) do
    attributes
    |> Enum.map(fn attr -> {attr.name, attr_to_type_spec(attr)} end)
    |> Map.new()
  end

  # Convert an attribute definition to a type specification for socket validation
  defp attr_to_type_spec(attr) do
    if attr.type == :map && attr.nested_attributes do
      # Handle nested attributes for map types
      nested_attrs =
        attr.nested_attributes
        |> Enum.map(fn nested -> {nested.name, attr_to_type_spec(nested)} end)
        |> Map.new()

      nested_attrs
    else
      # Handle basic types
      attr.type
    end
  end
end
