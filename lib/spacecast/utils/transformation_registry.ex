defmodule Spacecast.Utils.TransformationRegistry do
  @moduledoc """
  Registry for discovering and managing resource transformations.

  This module provides functionality for registering, discovering, and retrieving
  transformation modules that can be used in transformation pipelines.

  ## Features

  - **Dynamic Registration**: Register transformations at runtime
  - **Auto-Discovery**: Discover transformation modules in specified paths
  - **Categorization**: Organize transformations by type and purpose
  - **Metadata**: Store and retrieve metadata about transformations

  ## Examples

  ```elixir
  # Register a transformation
  TransformationRegistry.register(MyApp.Transformations.NormalizeEmail)

  # Get all registered transformations
  transformations = TransformationRegistry.list_all()

  # Get transformations by category
  email_transforms = TransformationRegistry.list_by_category(:email)

  # Discover transformations in a module
  TransformationRegistry.discover_in_module(MyApp.Transformations)
  ```
  """

  use GenServer

  @registry_name __MODULE__

  # Client API

  @doc """
  Starts the transformation registry.

  ## Options

  - `:name` - The name to register the registry process under
  - `:auto_discover` - Whether to auto-discover transformations at startup (default: true)
  - `:discover_paths` - Module paths to search for transformations

  ## Examples

  ```elixir
  # Start with default options
  {:ok, pid} = TransformationRegistry.start_link()

  # Start with custom options
  {:ok, pid} = TransformationRegistry.start_link(
    name: MyRegistry,
    auto_discover: false,
    discover_paths: [MyApp.Transformations]
  )
  ```
  """
  def start_link(opts \\ []) do
    name = Keyword.get(opts, :name, @registry_name)
    GenServer.start_link(__MODULE__, opts, name: name)
  end

  @doc """
  Registers a transformation module with the registry.

  ## Examples

  ```elixir
  # Register a transformation
  TransformationRegistry.register(MyApp.Transformations.NormalizeEmail)

  # Register with categories
  TransformationRegistry.register(MyApp.Transformations.NormalizeEmail, categories: [:email, :normalization])

  # Register with metadata
  TransformationRegistry.register(
    MyApp.Transformations.NormalizeEmail,
    metadata: %{
      description: "Normalizes email addresses to lowercase",
      author: "John Doe"
    }
  )
  ```
  """
  def register(module, opts \\ []) do
    server = Keyword.get(opts, :registry, @registry_name)
    categories = Keyword.get(opts, :categories, [])
    metadata = Keyword.get(opts, :metadata, %{})

    GenServer.call(server, {:register, module, categories, metadata})
  end

  @doc """
  Unregisters a transformation module from the registry.

  ## Examples

  ```elixir
  # Unregister a transformation
  TransformationRegistry.unregister(MyApp.Transformations.NormalizeEmail)
  ```
  """
  def unregister(module, opts \\ []) do
    server = Keyword.get(opts, :registry, @registry_name)
    GenServer.call(server, {:unregister, module})
  end

  @doc """
  Lists all registered transformation modules.

  ## Examples

  ```elixir
  # Get all transformations
  transformations = TransformationRegistry.list_all()
  ```

  ## Returns

  List of transformation module entries, each with the format:

  ```
  %{
    module: ModuleName,
    categories: [:category1, :category2],
    metadata: %{key: value}
  }
  ```
  """
  def list_all(opts \\ []) do
    server = Keyword.get(opts, :registry, @registry_name)
    GenServer.call(server, :list_all)
  end

  @doc """
  Lists transformation modules filtered by category.

  ## Examples

  ```elixir
  # Get email-related transformations
  email_transforms = TransformationRegistry.list_by_category(:email)
  ```

  ## Returns

  List of transformation module entries in the specified category.
  """
  def list_by_category(category, opts \\ []) do
    server = Keyword.get(opts, :registry, @registry_name)
    GenServer.call(server, {:list_by_category, category})
  end

  @doc """
  Gets information about a specific transformation module.

  ## Examples

  ```elixir
  # Get info about a transformation
  info = TransformationRegistry.get_info(MyApp.Transformations.NormalizeEmail)
  ```

  ## Returns

  Entry for the specified transformation module, or nil if not found.
  """
  def get_info(module, opts \\ []) do
    server = Keyword.get(opts, :registry, @registry_name)
    GenServer.call(server, {:get_info, module})
  end

  @doc """
  Discovers transformation modules in a module or path.

  This function searches the specified module and its submodules for modules
  that use the `Spacecast.Utils.Transformation` behaviour.

  ## Examples

  ```elixir
  # Discover transformations in a module
  TransformationRegistry.discover_in_module(MyApp.Transformations)
  ```

  ## Returns

  List of discovered transformation modules.
  """
  def discover_in_module(module, opts \\ []) do
    server = Keyword.get(opts, :registry, @registry_name)
    GenServer.call(server, {:discover_in_module, module})
  end

  @doc """
  Discovers transformation modules in the application.

  This function searches all modules in the application for modules
  that use the `Spacecast.Utils.Transformation` behaviour.

  ## Examples

  ```elixir
  # Discover all transformations in the application
  TransformationRegistry.discover_all()
  ```

  ## Returns

  List of discovered transformation modules.
  """
  def discover_all(opts \\ []) do
    server = Keyword.get(opts, :registry, @registry_name)
    GenServer.call(server, :discover_all)
  end

  @doc """
  Clears all registered transformations.

  ## Examples

  ```elixir
  # Clear the registry
  TransformationRegistry.clear()
  ```
  """
  def clear(opts \\ []) do
    server = Keyword.get(opts, :registry, @registry_name)
    GenServer.call(server, :clear)
  end

  # Server callbacks

  @impl true
  def init(opts) do
    auto_discover = Keyword.get(opts, :auto_discover, true)
    discover_paths = Keyword.get(opts, :discover_paths, [])

    # Initialize the registry
    state = %{
      transformations: %{},
      categories: %{}
    }

    # Auto-discover transformations if enabled
    state =
      if auto_discover do
        # Discover in specified paths
        state =
          Enum.reduce(discover_paths, state, fn path, acc ->
            {:ok, modules} = do_discover_in_module(path)
            register_modules(modules, acc)
          end)

        # Discover all if no paths specified
        if Enum.empty?(discover_paths) do
          {:ok, modules} = do_discover_all()
          register_modules(modules, state)
        else
          state
        end
      else
        state
      end

    {:ok, state}
  end

  @impl true
  def handle_call({:register, module, categories, metadata}, _from, state) do
    # Check if the module is a valid transformation
    if is_valid_transformation?(module) do
      # Add to transformations
      transformations =
        Map.put(state.transformations, module, %{
          module: module,
          categories: categories,
          metadata: metadata
        })

      # Add to category indexes
      categories =
        Enum.reduce(categories, state.categories, fn category, acc ->
          modules = Map.get(acc, category, [])
          Map.put(acc, category, [module | modules])
        end)

      {:reply, :ok, %{state | transformations: transformations, categories: categories}}
    else
      {:reply, {:error, "Not a valid transformation module"}, state}
    end
  end

  @impl true
  def handle_call({:unregister, module}, _from, state) do
    # Check if the module is registered
    if Map.has_key?(state.transformations, module) do
      # Get the module's categories
      categories = state.transformations[module].categories

      # Remove from transformations
      transformations = Map.delete(state.transformations, module)

      # Remove from category indexes
      categories_map =
        Enum.reduce(categories, state.categories, fn category, acc ->
          modules = Map.get(acc, category, [])
          Map.put(acc, category, Enum.filter(modules, &(&1 != module)))
        end)

      {:reply, :ok, %{state | transformations: transformations, categories: categories_map}}
    else
      {:reply, {:error, "Module not registered"}, state}
    end
  end

  @impl true
  def handle_call(:list_all, _from, state) do
    {:reply, Map.values(state.transformations), state}
  end

  @impl true
  def handle_call({:list_by_category, category}, _from, state) do
    modules = Map.get(state.categories, category, [])

    # Get the full entries for each module
    entries =
      Enum.map(modules, fn module ->
        Map.get(state.transformations, module)
      end)
      |> Enum.filter(&(&1 != nil))

    {:reply, entries, state}
  end

  @impl true
  def handle_call({:get_info, module}, _from, state) do
    info = Map.get(state.transformations, module)
    {:reply, info, state}
  end

  @impl true
  def handle_call({:discover_in_module, module}, _from, state) do
    case do_discover_in_module(module) do
      {:ok, modules} ->
        # Register the discovered modules
        updated_state = register_modules(modules, state)
        {:reply, {:ok, modules}, updated_state}

      {:error, reason} ->
        {:reply, {:error, reason}, state}
    end
  end

  @impl true
  def handle_call(:discover_all, _from, state) do
    case do_discover_all() do
      {:ok, modules} ->
        # Register the discovered modules
        updated_state = register_modules(modules, state)
        {:reply, {:ok, modules}, updated_state}

      {:error, reason} ->
        {:reply, {:error, reason}, state}
    end
  end

  @impl true
  def handle_call(:clear, _from, _state) do
    # Reset the registry
    state = %{
      transformations: %{},
      categories: %{}
    }

    {:reply, :ok, state}
  end

  # Helper functions

  # Check if a module is a valid transformation
  defp is_valid_transformation?(module) do
    # Check if the module exists and implements the Transformation behaviour
    Code.ensure_loaded?(module) and
      function_exported?(module, :transform, 2) and
      function_exported?(module, :applicable?, 2)
  end

  # Discover transformations in a module
  defp do_discover_in_module(root_module) do
    try do
      # Get all modules in the application
      {:ok, modules} =
        :application.get_key(:kernel, :modules)

      # Filter to modules that start with the root module name
      root_prefix = Atom.to_string(root_module)

      modules =
        modules
        |> Enum.filter(fn module ->
          module_str = Atom.to_string(module)
          String.starts_with?(module_str, root_prefix)
        end)
        |> Enum.filter(&is_valid_transformation?/1)

      {:ok, modules}
    rescue
      e -> {:error, "Error discovering modules: #{inspect(e)}"}
    end
  end

  # Discover all transformations
  defp do_discover_all do
    try do
      # Get all modules in the application
      {:ok, modules} =
        :application.get_key(:kernel, :modules)

      # Filter to valid transformation modules
      modules =
        modules
        |> Enum.filter(&is_valid_transformation?/1)

      {:ok, modules}
    rescue
      e -> {:error, "Error discovering modules: #{inspect(e)}"}
    end
  end

  # Register multiple modules
  defp register_modules(modules, state) do
    Enum.reduce(modules, state, fn module, acc ->
      # Extract categories from module info if available
      categories =
        if function_exported?(module, :info, 0) do
          info = module.info()
          Map.get(info, :categories, [])
        else
          []
        end

      # Extract metadata from module info if available
      metadata =
        if function_exported?(module, :info, 0) do
          info = module.info()
          Map.get(info, :metadata, %{})
        else
          %{}
        end

      # Add to transformations
      transformations =
        Map.put(acc.transformations, module, %{
          module: module,
          categories: categories,
          metadata: metadata
        })

      # Add to category indexes
      categories_map =
        Enum.reduce(categories, acc.categories, fn category, cat_acc ->
          modules = Map.get(cat_acc, category, [])
          Map.put(cat_acc, category, [module | modules])
        end)

      %{acc | transformations: transformations, categories: categories_map}
    end)
  end
end
