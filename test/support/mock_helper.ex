defmodule SpacecastWeb.TestMockHelper do
  @moduledoc """
  Helper for mocking external dependencies in tests.

  This module provides utilities for creating and using mocks in tests,
  particularly for external services and APIs that should not be called
  during testing.

  ## Usage

  ```elixir
  # In your test setup
  setup do
    MockHelper.setup_mocks()
    :ok
  end

  # In your test
  test "external API call is mocked", %{conn: conn} do
    MockHelper.expect_api_call(:external_service, :get_data, fn _ ->
      {:ok, %{"result" => "mocked data"}}
    end)

    # Test with mocked API
  end
  ```

  """

  import Mox

  @doc """
  Sets up mocks for external dependencies.

  Call this in your test setup to ensure mocks are properly configured.
  """
  def setup_mocks do
    # Reset all mocks before each test
    Mox.stub_with(Spacecast.MockHTTPClient, Spacecast.DefaultHTTPClient)
    Mox.stub_with(Spacecast.MockExternalAPI, Spacecast.DefaultExternalAPI)
    Mox.stub_with(Spacecast.MockSecurityLogger, Spacecast.DefaultSecurityLogger)
    Mox.stub_with(Spacecast.MockSecurityDetector, Spacecast.DefaultSecurityDetector)
    Mox.stub_with(Spacecast.MockSecurityAlerting, Spacecast.DefaultSecurityAlerting)

    Mox.stub_with(
      Spacecast.MockPerformanceMonitor,
      Spacecast.DefaultPerformanceMonitor
    )

    Mox.stub_with(Spacecast.MockPerformanceCache, Spacecast.DefaultPerformanceCache)

    # Set up RepoMock with flexible stub for any resource ID
    setup_repo_mock()

    :ok
  end

  @doc """
  Sets up the RepoMock with stubs for all necessary functions.
  """
  def setup_repo_mock do
    :ets.delete_all_objects(:mock_resources)

    Spacecast.RepoMock
    |> setup_insert_stubs()
    |> setup_update_stubs()
    |> setup_delete_stubs()
    |> setup_query_stubs()
    |> setup_transaction_stubs()
  end

  defp setup_insert_stubs(mock) do
    mock
    |> stub(:insert, &handle_insert/1)
    |> stub(:insert, &handle_insert_with_opts/2)
  end

  defp setup_update_stubs(mock) do
    mock
    |> stub(:update, &handle_update/1)
    |> stub(:update, &handle_update_with_opts/2)
  end

  defp setup_delete_stubs(mock) do
    mock
    |> stub(:delete, &handle_delete/1)
    |> stub(:delete, &handle_delete_with_opts/2)
    |> stub(:delete_all, &handle_delete_all/2)
  end

  defp setup_query_stubs(mock) do
    mock
    |> stub(:all, &handle_all/1)
    |> stub(:all, &handle_all_with_opts/2)
    |> stub(:get, &handle_get/3)
    |> stub(:get!, &handle_get!/3)
    |> stub(:get_by, &handle_get_by/3)
    |> stub(:one, &handle_one/2)
    |> stub(:aggregate, &handle_aggregate/4)
    |> stub(:exists?, &handle_exists?/2)
  end

  defp setup_transaction_stubs(mock) do
    mock
    |> stub(:transaction, &handle_transaction/2)
    |> stub(:rollback, &handle_rollback/1)
  end

  defp handle_insert(changeset) do
    if changeset.valid? do
      case changeset.data.__struct__ do
        Spacecast.Resources.Resource ->
          resource = build_resource_from_changeset(changeset)
          :ets.insert(:mock_resources, {resource.id, resource})
          {:ok, resource}

        _ ->
          # For any other struct, just return the changeset data
          {:ok, changeset.data}
      end
    else
      {:error, changeset}
    end
  end

  defp handle_insert_with_opts(changeset, _opts), do: handle_insert(changeset)

  defp handle_update(changeset) do
    if changeset.valid? do
      case changeset.data.__struct__ do
        Spacecast.Resources.Resource ->
          resource = update_resource_from_changeset(changeset)
          :ets.insert(:mock_resources, {resource.id, resource})
          {:ok, resource}

        _ ->
          # For any other struct, just return the changeset data
          {:ok, changeset.data}
      end
    else
      {:error, changeset}
    end
  end

  defp handle_update_with_opts(changeset, _opts), do: handle_update(changeset)

  defp handle_delete(resource) do
    case resource.__struct__ do
      Spacecast.Resources.Resource ->
        :ets.delete(:mock_resources, resource.id)
        {:ok, resource}

      _ ->
        # For any other struct, just return success
        {:ok, resource}
    end
  end

  defp handle_delete_with_opts(resource, _opts), do: handle_delete(resource)

  defp handle_delete_all(module, _opts_or_list) do
    case module do
      Spacecast.Resources.Resource ->
        :ets.delete_all_objects(:mock_resources)
        {0, nil}

      _ ->
        # For any other module, return {0, nil}
        {0, nil}
    end
  end

  defp handle_all(module) do
    case module do
      Spacecast.Resources.Resource ->
        :ets.tab2list(:mock_resources)
        |> Enum.map(fn {_id, resource} -> resource end)

      _ ->
        # For any other module, return an empty list
        []
    end
  end

  defp handle_all_with_opts(module, _opts_or_list), do: handle_all(module)

  defp handle_get(module, id, _opts_or_list) do
    case module do
      Spacecast.Resources.Resource ->
        case :ets.lookup(:mock_resources, id) do
          [{^id, resource}] -> resource
          [] -> nil
        end

      _ ->
        # For any other module, return nil
        nil
    end
  end

  defp handle_get!(module, id, _opts_or_list) do
    case module do
      Spacecast.Resources.Resource ->
        case :ets.lookup(:mock_resources, id) do
          [{^id, resource}] -> resource
          [] -> raise Ecto.QueryError, message: "Record not found"
        end

      _ ->
        # For any other module, raise not found error
        raise Ecto.QueryError, message: "Record not found"
    end
  end

  defp handle_get_by(module, _clauses, _opts_or_list) do
    case module do
      Spacecast.Resources.Resource ->
        nil

      _ ->
        # For any other module, return nil
        nil
    end
  end

  defp handle_one(module, _opts_or_list) do
    case module do
      Spacecast.Resources.Resource ->
        nil

      _ ->
        # For any other module, return nil
        nil
    end
  end

  defp handle_aggregate(module, _aggregate, _field, _opts_or_list) do
    case module do
      Spacecast.Resources.Resource ->
        0

      _ ->
        # For any other module, return 0
        0
    end
  end

  defp handle_exists?(module, _opts_or_list) do
    case module do
      Spacecast.Resources.Resource ->
        false

      _ ->
        # For any other module, return false
        false
    end
  end

  defp handle_transaction(fun, _opts_or_list), do: fun.()
  defp handle_rollback(value), do: {:error, value}

  defp build_resource_from_changeset(changeset) do
    %Spacecast.Resources.Resource{
      id: Ecto.UUID.generate(),
      name: changeset.changes[:name] || "Test Resource",
      description: changeset.changes[:description] || "A test resource",
      type: changeset.changes[:type] || "document",
      status: changeset.changes[:status] || "published",
      content: changeset.changes[:content] || %{text: "Test content"},
      metadata: changeset.changes[:metadata] || %{},
      settings: changeset.changes[:settings] || %{},
      version: changeset.changes[:version] || 1,
      parent_id: Map.get(changeset.changes, :parent_id, nil),
      child_ids: changeset.changes[:child_ids] || [],
      tags: changeset.changes[:tags] || [],
      categories: changeset.changes[:categories] || [],
      created_by: changeset.changes[:created_by],
      updated_by: changeset.changes[:updated_by],
      inserted_at: DateTime.utc_now(),
      updated_at: DateTime.utc_now()
    }
  end

  defp update_resource_from_changeset(changeset) do
    %{
      changeset.data
      | name: changeset.changes[:name] || changeset.data.name,
        description: changeset.changes[:description] || changeset.data.description,
        type: changeset.changes[:type] || changeset.data.type,
        status: changeset.changes[:status] || changeset.data.status,
        content: changeset.changes[:content] || changeset.data.content,
        metadata: changeset.changes[:metadata] || changeset.data.metadata,
        settings: changeset.changes[:settings] || changeset.data.settings,
        version: changeset.changes[:version] || changeset.data.version,
        parent_id: Map.get(changeset.changes, :parent_id, changeset.data.parent_id),
        child_ids: changeset.changes[:child_ids] || changeset.data.child_ids,
        tags: changeset.changes[:tags] || changeset.data.tags,
        categories: changeset.changes[:categories] || changeset.data.categories,
        created_by: changeset.changes[:created_by] || changeset.data.created_by,
        updated_by: changeset.changes[:updated_by] || changeset.data.updated_by,
        updated_at: DateTime.utc_now()
    }
  end

  @doc """
  Expects an API call to be made with the given parameters and returns the specified result.

  ## Parameters

  - `service` - The service being mocked (e.g., `:external_service`)
  - `action` - The action being performed (e.g., `:get_data`)
  - `callback` - A function that takes the parameters and returns a mocked result

  ## Example

  ```elixir
  MockHelper.expect_api_call(:external_service, :get_data, fn _ ->
    {:ok, %{"result" => "mocked data"}}
  end)
  ```
  """
  def expect_api_call(service, action, callback) when is_function(callback) do
    mock =
      case service do
        :external_api -> Spacecast.MockExternalAPI
        :http_client -> Spacecast.MockHTTPClient
        m when is_atom(m) -> m
        m -> m
      end

    Mox.expect(mock, action, callback)
  end

  @doc """
  Verifies that all expected calls were made.

  Call this at the end of your test to ensure all expected mocked calls were made.
  """
  def verify_all_mocks do
    verify!(Spacecast.MockHTTPClient)
    verify!(Spacecast.MockExternalAPI)
  end
end
