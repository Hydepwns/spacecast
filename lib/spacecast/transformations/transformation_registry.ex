defmodule Spacecast.Transformations.TransformationRegistry do
  @moduledoc """
  Registry for resource transformations.

  The TransformationRegistry is responsible for registering, organizing, and
  retrieving transformations that can be applied to resources during operations
  like create, update, and delete.

  ## Features

  - Registration of transformations with metadata (resource type, operation, etc.)
  - Discovery of applicable transformations for given scenarios
  - Dependency and priority-based sorting of transformations
  - Conditional transformation application
  """

  use GenServer

  alias Spacecast.Transformations.TransformationContext

  # Transformation record structure
  @type transformation :: %{
          name: String.t(),
          transformer: (map(), TransformationContext.t() ->
                          {:ok, map()}
                          | {:ok, map(), TransformationContext.t()}
                          | {:error, String.t()}
                          | {:error, String.t(), map()}),
          resource_type: module() | :any,
          operation: TransformationContext.operation() | :any,
          phase: TransformationContext.phase() | :any,
          dependencies: list(String.t()),
          condition: (map(), TransformationContext.t() -> boolean()),
          priority: integer(),
          metadata: map()
        }

  # Default options for transformation registration without the function
  @default_registration_opts [
    resource_type: :any,
    operation: :any,
    phase: :any,
    dependencies: [],
    priority: 50,
    metadata: %{}
  ]

  # Default condition function that can't be part of the attribute
  defp default_condition(_resource, _context), do: true

  # Start the registry
  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  # Initialize the registry state
  def init(_opts) do
    {:ok, %{transformations: []}}
  end

  @doc """
  Registers a transformation with the registry.

  ## Parameters

  - `name` - Unique name for the transformation
  - `transformer` - Function that applies the transformation
  - `opts` - Registration options:
      - `:resource_type` - Type of resource this applies to (module or :any)
      - `:operation` - Operation type (:create, :update, :delete, or :any)
      - `:phase` - When to apply (:before_validation, :after_validation, or :any)
      - `:dependencies` - List of transformation names this depends on
      - `:condition` - Function that determines if transformation should be applied
      - `:priority` - Execution priority (lower numbers run first)
      - `:metadata` - Additional metadata for the transformation

  ## Returns

  `:ok` if registration successful, `{:error, reason}` otherwise

  ## Example

  ```elixir
  TransformationRegistry.register(
    "format_email",
    &StandardTransformers.format_email/2,
    resource_type: User,
    operation: :create,
    priority: 10
  )
  ```
  """
  def register(name, transformer, opts \\ []) do
    GenServer.call(__MODULE__, {:register, name, transformer, opts})
  end

  @doc """
  Gets all transformations applicable to a resource type, operation, and phase.

  ## Parameters

  - `resource_type` - The type of resource
  - `operation` - The operation being performed (:create, :update, :delete)
  - `phase` - The phase of the operation (:before_validation, :after_validation)

  ## Returns

  List of applicable transformations, sorted by dependencies and priority

  ## Example

  ```elixir
  transformations = TransformationRegistry.get_transformations(
    User,
    :create,
    :before_validation
  )
  ```
  """
  def get_transformations(resource_type, operation, phase) do
    transformations = GenServer.call(__MODULE__, :get_transformations)

    transformations
    |> filter_transformations(resource_type, operation, phase)
    |> sort_by_dependencies()
  end

  @doc """
  Lists all registered transformations matching a filter function.

  ## Parameters

  - `filter_fn` - Function to filter transformations

  ## Returns

  List of matching transformations

  ## Example

  ```elixir
  email_transformers = TransformationRegistry.list_transformations(fn t -> 
    String.contains?(t.name, "email") 
  end)
  ```
  """
  def list_transformations(filter_fn \\ fn _ -> true end) do
    GenServer.call(__MODULE__, {:list, filter_fn})
  end

  @doc """
  Sorts transformations by dependencies and priority.

  This ensures that transformations run in the correct order, respecting
  any dependency relationships between them.

  ## Parameters

  - `transformations` - List of transformations to sort

  ## Returns

  Sorted list of transformations
  """
  def sort_by_dependencies(transformations) do
    # First, build a dependency graph
    transformation_map = Map.new(transformations, &{&1.name, &1})
    dependency_graph = build_dependency_graph(transformations, transformation_map)

    # Then perform a topological sort
    sorted_names = topological_sort(dependency_graph)

    # Finally, sort by the topological order and then by priority
    transformations
    |> Enum.sort_by(
      fn transformation ->
        # Find index in the topological sort (or infinity if not found)
        topo_index =
          Enum.find_index(sorted_names, fn name -> name == transformation.name end) ||
            length(sorted_names)

        # Create a composite sort key: [topological_index, priority]
        {topo_index, transformation.priority}
      end,
      # Sort ascending
      &(&1 <= &2)
    )
  end

  # Server callbacks

  def handle_call({:register, name, transformer, opts}, _from, state) do
    # Merge provided options with defaults
    opts = Keyword.merge(@default_registration_opts, opts)

    # Apply the default condition function if not provided
    opts =
      if Keyword.has_key?(opts, :condition),
        do: opts,
        else: Keyword.put(opts, :condition, &default_condition/2)

    # Create the transformation record
    transformation = %{
      name: name,
      transformer: transformer,
      resource_type: Keyword.get(opts, :resource_type),
      operation: Keyword.get(opts, :operation),
      phase: Keyword.get(opts, :phase),
      dependencies: Keyword.get(opts, :dependencies),
      condition: Keyword.get(opts, :condition),
      priority: Keyword.get(opts, :priority),
      metadata: Keyword.get(opts, :metadata)
    }

    # Add to state if not already present, or replace if exists
    transformations =
      Enum.reject(state.transformations, fn t -> t.name == name end) ++ [transformation]

    {:reply, :ok, %{state | transformations: transformations}}
  end

  def handle_call(:get_transformations, _from, state) do
    {:reply, state.transformations, state}
  end

  def handle_call({:list, filter_fn}, _from, state) do
    filtered_transformations = Enum.filter(state.transformations, filter_fn)
    {:reply, filtered_transformations, state}
  end

  # Helper functions

  # Filter transformations by resource type, operation, and phase
  defp filter_transformations(transformations, resource_type, operation, phase) do
    Enum.filter(transformations, fn transformation ->
      matches_resource_type?(transformation.resource_type, resource_type) &&
        matches_operation?(transformation.operation, operation) &&
        matches_phase?(transformation.phase, phase)
    end)
  end

  # Match exact types first, then :any, then fallback
  defp matches_resource_type?(type, type), do: true
  defp matches_resource_type?(:any, _), do: true
  defp matches_resource_type?(_type, _target), do: false

  # Match exact operations first, then :any, then fallback
  defp matches_operation?(op, op), do: true
  defp matches_operation?(:any, _), do: true
  defp matches_operation?(_op, _target), do: false

  # Match exact phases first, then :any, then fallback
  defp matches_phase?(phase, phase), do: true
  defp matches_phase?(:any, _), do: true
  defp matches_phase?(_phase, _target), do: false

  # Build a dependency graph for topological sorting
  defp build_dependency_graph(transformations, transformation_map) do
    Enum.reduce(transformations, %{}, fn transformation, graph ->
      # Get dependencies that actually exist in the transformation list
      deps =
        transformation.dependencies
        |> Enum.filter(&Map.has_key?(transformation_map, &1))

      # Add this node to the graph
      Map.put(graph, transformation.name, deps)
    end)
  end

  # Perform a topological sort on the dependency graph
  defp topological_sort(graph) do
    # Find all nodes with no dependencies
    no_deps =
      graph
      |> Enum.filter(fn {_node, deps} -> Enum.empty?(deps) end)
      |> Enum.map(fn {node, _deps} -> node end)

    # Sort the rest of the graph
    topological_sort_visit(graph, no_deps, [])
  end

  # Recursive topological sort implementation
  defp topological_sort_visit(_graph, [], sorted), do: Enum.reverse(sorted)

  defp topological_sort_visit(graph, nodes, sorted) do
    case nodes do
      [] ->
        Enum.reverse(sorted)

      [node | rest] ->
        # Remove this node from the dependency lists of all other nodes
        new_graph =
          Enum.map(graph, fn {n, deps} ->
            {n, Enum.reject(deps, &(&1 == node))}
          end)
          |> Map.new()

        # Find newly dependency-free nodes
        new_no_deps =
          new_graph
          |> Enum.filter(fn {n, deps} -> n != node && Enum.empty?(deps) end)
          |> Enum.map(fn {n, _deps} -> n end)
          |> Enum.reject(&(&1 in sorted))
          |> Enum.reject(&(&1 in rest))

        # Continue sorting
        topological_sort_visit(new_graph, rest ++ new_no_deps, [node | sorted])
    end
  end
end
