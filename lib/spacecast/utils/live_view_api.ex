defmodule Spacecast.Utils.LiveViewAPI do
  @moduledoc """
  Provides an API-style interface for LiveView socket assigns.

  This module implements standardized API methods for accessing and manipulating
  LiveView socket assigns in a consistent way, inspired by Ash Framework's API patterns.

  ## Features

  - Standardized methods for reading, updating, and managing socket assigns
  - Consistent error handling and validation
  - Domain-oriented organization of LiveView functionality
  - Support for resource-oriented socket assigns

  ## Usage

  ```elixir
  # In a LiveView module
  alias Spacecast.Utils.LiveViewAPI

  def handle_event("update_user", %{"user" => user_params}, socket) do
    case LiveViewAPI.update(socket, :user, user_params) do
      {:ok, updated_socket} ->
        {:noreply, updated_socket}
      
      {:error, message, socket} ->
        {:noreply, put_flash(socket, :error, message)}
    end
  end

  def handle_event("get_user_role", _, socket) do
    role = LiveViewAPI.get(socket, :user, :role)
    
    # Do something with the role...
    
    {:noreply, socket}
  end
  """

  alias Phoenix.LiveView.Socket
  alias Spacecast.Utils.LiveViewResource
  alias Spacecast.Utils.ChangeTracker
  alias Spacecast.Utils.SocketValidator

  @doc """
  Gets a value directly from socket assigns.

  ## Parameters

  - `socket` - The LiveView socket.
  - `field` - The field to get from the assigns.
  - `default` - Optional default value to return if the field doesn't exist.

  ## Returns

  The value of the specified field, or the default value if it doesn't exist.

  ## Examples

  ```elixir
  # Get the current theme
  theme = LiveViewAPI.get(socket, :theme, "light")
  ```
  """
  def get(%Socket{} = socket, field, default \\ nil) when is_atom(field) do
    Map.get(socket.assigns, field, default)
  end

  @doc """
  Gets a value directly from socket assigns (alias for get/2).

  ## Parameters

  - `socket` - The LiveView socket.
  - `field` - The field to get from the assigns.

  ## Returns

  The value of the specified field, or nil if it doesn't exist.

  ## Examples

  ```elixir
  # Get the current user
  user = LiveViewAPI.get_assign(socket, :user)
  ```
  """
  def get_assign(%Socket{} = socket, field) when is_atom(field) do
    Map.get(socket.assigns, field)
  end

  @doc """
  Updates a specific field in socket assigns.

  ## Parameters

  - `socket` - The LiveView socket.
  - `field` - The field to update.
  - `value` - The new value for the field.
  - `opts` - Options for the update operation.
    - `:validate` - Whether to validate the value before updating. Defaults to `true`.

  ## Returns

  - `{:ok, socket}` - If the update was successful.
  - `{:error, message, socket}` - If the update failed.

  ## Examples

  ```elixir
  # Update a single field
  case LiveViewAPI.update_field(socket, :theme, "dark") do
    {:ok, updated_socket} ->
      {:noreply, updated_socket}
    
    {:error, message, socket} ->
      {:noreply, put_flash(socket, :error, message)}
  end
  ```
  """
  def update_field(%Socket{} = socket, field, value, opts \\ []) when is_atom(field) do
    validate = Keyword.get(opts, :validate, true)

    if validate do
      case validate_single_field_update(socket, field, value) do
        {:ok, validated_value} ->
          {:ok, Phoenix.Component.assign(socket, field, validated_value)}

        {:error, message} ->
          {:error, message, socket}
      end
    else
      {:ok, Phoenix.Component.assign(socket, field, value)}
    end
  end

  @doc """
  Gets a value from a resource-oriented socket assign.

  ## Parameters

  - `socket` - The LiveView socket.
  - `resource` - The resource key in the socket assigns.
  - `field` - The field within the resource to get.
  - `default` - Optional default value to return if the field doesn't exist.

  ## Returns

  The value of the specified field, or the default value if it doesn't exist.

  ## Examples

  ```elixir
  # Get the user's role
  role = LiveViewAPI.get_resource(socket, :user, :role)

  # Get the user's theme with a default
  theme = LiveViewAPI.get_resource(socket, :user, :theme, "light")
  ```
  """
  def get_resource(%Socket{} = socket, resource, field, default \\ nil)
      when is_atom(resource) and is_atom(field) do
    socket.assigns
    |> Map.get(resource, %{})
    |> Map.get(field, default)
  end

  @doc """
  Updates socket assigns directly with the given values.

  ## Parameters

  - `socket` - The LiveView socket.
  - `values` - A map of field-value pairs to update.
  - `opts` - Options for the update operation.
    - `:validate` - Whether to validate the values before updating. Defaults to `true`.

  ## Returns

  - `{:ok, socket}` - If the update was successful.
  - `{:error, message, socket}` - If the update failed.

  ## Examples

  ```elixir
  # Update multiple assigns at once
  case LiveViewAPI.update(socket, %{theme: "dark", sidebar_open: true}) do
    {:ok, updated_socket} ->
      {:noreply, updated_socket}
    
    {:error, message, socket} ->
      {:noreply, put_flash(socket, :error, message)}
  end
  ```
  """
  def update(%Socket{} = socket, values, opts \\ []) when is_map(values) do
    validate = Keyword.get(opts, :validate, true)

    if validate do
      case validate_assigns_update(socket, values) do
        {:ok, validated_values} ->
          {:ok, Phoenix.Component.assign(socket, validated_values)}

        {:error, message} ->
          {:error, message, socket}
      end
    else
      {:ok, Phoenix.Component.assign(socket, values)}
    end
  end

  @doc """
  Updates a resource in the socket assigns with the given values.

  ## Parameters

  - `socket` - The LiveView socket.
  - `resource` - The resource key in the socket assigns.
  - `values` - A map of field-value pairs to update.
  - `opts` - Options for the update operation.
    - `:validate` - Whether to validate the values before updating. Defaults to `true`.

  ## Returns

  - `{:ok, socket}` - If the update was successful.
  - `{:error, message, socket}` - If the update failed.

  ## Examples

  ```elixir
  # Update the user's settings
  case LiveViewAPI.update_resource(socket, :user, %{role: "admin", active: true}) do
    {:ok, updated_socket} ->
      {:noreply, updated_socket}
    
    {:error, message, socket} ->
      {:noreply, put_flash(socket, :error, message)}
  end
  ```
  """
  def update_resource(%Socket{} = socket, resource, values, opts \\ [])
      when is_atom(resource) do
    validate = Keyword.get(opts, :validate, true)
    atom_keyed_values = atomize_map_keys(values)

    if validate do
      case validate_resource_update(socket, resource, atom_keyed_values) do
        {:ok, validated_values} ->
          {:ok, Phoenix.Component.assign(socket, resource, validated_values)}

        {:error, message} ->
          {:error, message, socket}
      end
    else
      {:ok, Phoenix.Component.assign(socket, resource, atom_keyed_values)}
    end
  end

  defp atomize_map_keys(values) when is_map(values) do
    Enum.reduce(values, %{}, fn {k, v}, acc ->
      if is_binary(k) do
        Map.put(acc, String.to_atom(k), v)
      else
        Map.put(acc, k, v)
      end
    end)
  end

  defp atomize_map_keys(values), do: values

  @doc """
  Creates a new resource in the socket assigns.

  ## Parameters

  - `socket` - The LiveView socket.
  - `resource` - The resource key in the socket assigns.
  - `values` - A map of field-value pairs for the resource.
  - `opts` - Options for the create operation.
    - `:validate` - Whether to validate the values before creating. Defaults to `true`.
    - `:replace` - Whether to replace an existing resource. Defaults to `false`.

  ## Returns

  - `{:ok, socket}` - If the create was successful.
  - `{:error, message, socket}` - If the create failed.

  ## Examples

  ```elixir
  # Create a new user resource
  case LiveViewAPI.create(socket, :user, %{id: "123", name: "John", role: "admin"}) do
    {:ok, updated_socket} ->
      {:noreply, updated_socket}
    
    {:error, message, socket} ->
      {:noreply, put_flash(socket, :error, message)}
  end
  ```
  """
  def create(%Socket{} = socket, resource, values, opts \\ [])
      when is_atom(resource) and is_map(values) do
    validate = Keyword.get(opts, :validate, true)
    replace = Keyword.get(opts, :replace, false)

    # Check if resource already exists
    existing_resource = Map.get(socket.assigns, resource)

    cond do
      existing_resource != nil and not replace ->
        {:error, "Resource #{resource} already exists", socket}

      validate ->
        case validate_resource_create(socket, resource, values) do
          {:ok, validated_values} ->
            {:ok, Phoenix.Component.assign(socket, resource, validated_values)}

          {:error, message} ->
            {:error, message, socket}
        end

      true ->
        {:ok, Phoenix.Component.assign(socket, resource, values)}
    end
  end

  @doc """
  Removes a resource from the socket assigns.

  ## Parameters

  - `socket` - The LiveView socket.
  - `resource` - The resource key in the socket assigns.

  ## Returns

  - `{:ok, socket}` - The updated socket with the resource removed.

  ## Examples

  ```elixir
  # Remove the user resource
  {:ok, socket} = LiveViewAPI.remove(socket, :user)
  ```
  """
  def remove(%Socket{} = socket, resource) when is_atom(resource) do
    updated_assigns = Map.delete(socket.assigns, resource)
    {:ok, %Socket{socket | assigns: updated_assigns}}
  end

  @doc """
  Gets all resources from a socket's assigns that match a pattern.

  ## Parameters

  - `socket` - The LiveView socket.
  - `pattern` - A regex pattern to match against resource names.

  ## Returns

  A map of resource names to resource values.

  ## Examples

  ```elixir
  # Get all user-related resources
  user_resources = LiveViewAPI.get_resources(socket, ~r/^user_/)
  ```
  """
  def get_resources(%Socket{} = socket, pattern) when is_struct(pattern, Regex) do
    socket.assigns
    |> Enum.filter(fn {key, _value} -> Regex.match?(pattern, Atom.to_string(key)) end)
    |> Map.new()
  end

  @doc """
  Creates a resource in the socket assigns from a LiveViewResource module.

  ## Parameters

  - `socket` - The LiveView socket.
  - `resource_key` - The key to assign the resource to in the socket.
  - `resource_module` - The LiveViewResource module to use.
  - `values` - A map of values to initialize the resource with.
  - `opts` - Options for the create operation.
    - `:validate` - Whether to validate the values. Defaults to `true`.

  ## Returns

  - `{:ok, socket}` - If the create was successful.
  - `{:error, message, socket}` - If the create failed.

  ## Examples

  ```elixir
  defmodule MyApp.UserResource do
    use Spacecast.Utils.LiveViewResource
    
    attributes do
      attribute :id, :string, required: true
      attribute :name, :string, required: true
    end
  end

  # Create a user resource from the module
  case LiveViewAPI.create_from_resource(socket, :user, MyApp.UserResource, %{id: "123", name: "John"}) do
    {:ok, updated_socket} ->
      {:noreply, updated_socket}
    
    {:error, message, socket} ->
      {:noreply, put_flash(socket, :error, message)}
  end
  ```
  """
  def create_from_resource(%Socket{} = socket, resource_key, resource_module, values, opts \\ [])
      when is_atom(resource_key) and is_atom(resource_module) and is_map(values) do
    validate = Keyword.get(opts, :validate, true)

    if validate do
      case validate_resource_values(resource_module, values) do
        {:ok, validated_values} ->
          {:ok, Phoenix.Component.assign(socket, resource_key, validated_values)}

        {:error, message} ->
          {:error, message, socket}
      end
    else
      {:ok, Phoenix.Component.assign(socket, resource_key, values)}
    end
  end

  defp validate_resource_values(resource_module, values) do
    type_specs = LiveViewResource.generate_type_specs(resource_module)
    validation_results = validate_values_against_specs(values, type_specs)

    if Enum.empty?(validation_results) do
      schema = resource_module.__resource_schema__()
      defaults = get_default_values(schema)
      {:ok, Map.merge(defaults, values)}
    else
      {:error, Enum.join(validation_results, "; ")}
    end
  end

  defp validate_values_against_specs(values, type_specs) do
    Enum.map(values, fn {key, value} ->
      validate_value_against_spec(key, value, type_specs)
    end)
    |> Enum.reject(&is_nil/1)
  end

  defp validate_value_against_spec(key, value, type_specs) do
    case Map.get(type_specs, key) do
      nil ->
        nil

      type_spec ->
        case SocketValidator.validate_type(value, type_spec) do
          :ok -> nil
          {:error, message} -> message
        end
    end
  end

  defp get_default_values(schema) do
    schema.attributes
    |> Enum.filter(fn attr -> attr.default != nil end)
    |> Enum.map(fn attr -> {attr.name, attr.default} end)
    |> Map.new()
  end

  @doc """
  Updates a resource in the socket assigns with change tracking.

  This function updates a resource and tracks the changes made, creating
  a change history entry. It also performs validation and optimistic concurrency control.

  ## Parameters

  - `socket` - The LiveView socket.
  - `resource_key` - The resource key in socket assigns.
  - `updates` - A map of updates to apply to the resource.
  - `metadata` - A map containing metadata about the change:
    - `:actor` - Who made the change (optional)
    - `:reason` - Why the change was made (optional)
    - `:source` - Where the change originated from (optional)
    - `:expected_version` - The expected version for optimistic concurrency control (optional)

  ## Returns

  - `{:ok, socket}` - If the update was successful.
  - `{:error, :stale_resource, socket}` - If the resource has been updated by someone else.
  - `{:error, message, socket}` - If the update failed due to validation.

  ## Examples

  ```elixir
  case LiveViewAPI.update_with_tracking(socket, :user, %{name: "New Name"}, %{
    actor: "admin@example.com",
    reason: "Name correction"
  }) do
    {:ok, updated_socket} ->
      {:noreply, updated_socket}
    
    {:error, :stale_resource, socket} ->
      {:noreply, put_flash(socket, :error, "Resource was updated by someone else")}
    
    {:error, message, socket} ->
      {:noreply, put_flash(socket, :error, message)}
  end
  ```
  """
  def update_with_tracking(%Socket{} = socket, resource_key, updates, metadata \\ %{})
      when is_atom(resource_key) and is_map(updates) do
    with {:ok, resource} <- get_resource(socket, resource_key),
         {:ok, validated_updates} <- validate_resource_update(socket, resource_key, updates),
         {:ok, updated_resource} <- apply_resource_update(resource, validated_updates, metadata) do
      {:ok, Phoenix.Component.assign(socket, resource_key, updated_resource)}
    else
      {:error, :stale_resource} -> {:error, :stale_resource, socket}
      {:error, message} -> {:error, message, socket}
    end
  end

  defp get_resource(socket, resource_key) do
    case Map.get(socket.assigns, resource_key) do
      nil -> {:error, "Resource #{resource_key} not found in socket assigns"}
      resource -> {:ok, resource}
    end
  end

  defp apply_resource_update(resource, updates, metadata) do
    resource_module = Map.get(resource, :__resource_module__)

    if is_nil(resource_module) do
      {:error, "Resource does not have a __resource_module__ attribute"}
    else
      enhanced_metadata = enhance_metadata_with_validation(metadata, resource_module)
      resource_module.update_with_tracking(resource, updates, enhanced_metadata)
    end
  end

  @doc """
  Gets the change history for a resource in the socket assigns.

  ## Parameters

  - `socket` - The LiveView socket.
  - `resource_key` - The resource key in socket assigns.
  - `opts` - Options for filtering the history:
    - `:limit` - Maximum number of changes to return
    - `:since` - Only return changes since this timestamp
    - `:until` - Only return changes until this timestamp
    - `:by_actor` - Only return changes by this actor

  ## Returns

  - `{:ok, changes}` - The change history
  - `{:error, reason}` - An error occurred

  ## Examples

  ```elixir
  case LiveViewAPI.get_history(socket, :user) do
    {:ok, changes} ->
      # Do something with the changes...
    
    {:error, reason} ->
      # Handle error...
  end
  ```
  """
  def get_history(%Socket{} = socket, resource_key, opts \\ [])
      when is_atom(resource_key) do
    # Get the resource
    resource = Map.get(socket.assigns, resource_key)

    if is_nil(resource) do
      {:error, "Resource #{resource_key} not found in socket assigns"}
    else
      ChangeTracker.get_history(resource, opts)
    end
  end

  @doc """
  Gets a specific version of a resource from the socket assigns.

  ## Parameters

  - `socket` - The LiveView socket.
  - `resource_key` - The resource key in socket assigns.
  - `version` - The version number to retrieve.

  ## Returns

  - `{:ok, versioned_resource}` - The resource at the specified version
  - `{:error, reason}` - An error occurred

  ## Examples

  ```elixir
  case LiveViewAPI.get_version(socket, :user, 1) do
    {:ok, previous_version} ->
      # Do something with the previous version...
    
    {:error, reason} ->
      # Handle error...
  end
  ```
  """
  def get_version(%Socket{} = socket, resource_key, version)
      when is_atom(resource_key) and is_integer(version) do
    # Get the resource
    resource = Map.get(socket.assigns, resource_key)

    if is_nil(resource) do
      {:error, "Resource #{resource_key} not found in socket assigns"}
    else
      ChangeTracker.get_version(resource, version)
    end
  end

  @doc """
  Creates a diff between two versions of a resource in the socket assigns.

  ## Parameters

  - `socket` - The LiveView socket.
  - `resource_key` - The resource key in socket assigns.
  - `opts` - Options for specifying the versions:
    - `:version1` - First version (default: previous version)
    - `:version2` - Second version (default: current version)

  ## Returns

  - `{:ok, diff}` - The diff between the two versions
  - `{:error, reason}` - An error occurred

  ## Examples

  ```elixir
  case LiveViewAPI.diff_versions(socket, :user, version1: 1, version2: 2) do
    {:ok, diff} ->
      # Do something with the diff...
    
    {:error, reason} ->
      # Handle error...
  end
  ```
  """
  def diff_versions(%Socket{} = socket, resource_key, opts \\ [])
      when is_atom(resource_key) do
    # Get the resource
    resource = Map.get(socket.assigns, resource_key)

    if is_nil(resource) do
      {:error, "Resource #{resource_key} not found in socket assigns"}
    else
      ChangeTracker.diff(resource, opts)
    end
  end

  # Extracts context validation logic to reduce nesting depth
  defp get_context_validation(resource_module) do
    if function_exported?(resource_module, :__context_validation__, 0) do
      resource_module.__context_validation__()
    else
      false
    end
  end

  # Enhances metadata with context validation to reduce nesting depth
  defp enhance_metadata_with_validation(metadata, resource_module) do
    context_validation = Map.get(metadata, :context_validation)

    if context_validation != nil do
      metadata
    else
      validation_value = get_context_validation(resource_module)
      Map.put(metadata, :context_validation, validation_value)
    end
  end

  # Private helper functions

  defp validate_resource_update(%Socket{} = _socket, resource_key, values) do
    if resource_key == :user do
      case Map.get(values, :role) do
        "user" ->
          {:ok, values}

        "admin" ->
          {:ok, values}

        nil ->
          {:ok, values}

        invalid_role ->
          allowed_roles = ["user", "admin"]

          error_message =
            "Invalid role for user: expected one of #{inspect(allowed_roles)} but got #{inspect(invalid_role)}"

          {:error, error_message}
      end
    else
      {:ok, values}
    end
  end

  defp validate_assigns_update(%Socket{} = _socket, values) do
    {:ok, values}
  end

  defp validate_resource_create(%Socket{} = _socket, _resource, values) do
    {:ok, values}
  end

  defp validate_single_field_update(%Socket{} = socket, field, value) do
    case field do
      :theme ->
        case SocketValidator.validate_type(value, :string) do
          :ok -> {:ok, value}
          {:error, message} -> {:error, message, socket}
        end

      :sidebar_open ->
        case SocketValidator.validate_type(value, :boolean) do
          :ok -> {:ok, value}
          {:error, message} -> {:error, message, socket}
        end

      _ ->
        {:ok, value}
    end
  end
end
