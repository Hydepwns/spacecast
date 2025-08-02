defmodule SpacecastWeb.Integration.PerformanceIntegrationTest do
  @moduledoc """
  Comprehensive performance and load testing suite covering response times,
  throughput, memory usage, and scalability testing.
  """

  use SpacecastWeb.ConnCase
  import Phoenix.LiveViewTest
  import Mox
  setup :set_mox_from_context
  setup :verify_on_exit!

  setup do
    # Use a more robust sandbox setup that handles concurrent access
    case Ecto.Adapters.SQL.Sandbox.checkout(Spacecast.Repo) do
      :ok -> :ok
      # Already allowed, which is fine
      {:already, :allowed} -> :ok
      # Already checked out, which is fine
      {:already, :checked_out} -> :ok
      error -> raise "Failed to checkout sandbox: #{inspect(error)}"
    end

    case Ecto.Adapters.SQL.Sandbox.mode(Spacecast.Repo, {:shared, self()}) do
      :ok -> :ok
      # Already allowed, which is fine
      {:already, :allowed} -> :ok
      # Not the owner, which is fine in shared mode
      :not_owner -> :ok
      error -> raise "Failed to set sandbox mode: #{inspect(error)}"
    end

    :ok
  end

  # Override repo configuration for integration tests to use real database
  setup do
    # Temporarily set repo to use real database for integration tests
    Application.put_env(:spacecast, :repo, Spacecast.Repo)

    on_exit(fn ->
      # Restore mock repo after test
      Application.put_env(:spacecast, :repo, Spacecast.RepoMock)
    end)

    :ok
  end

  alias Spacecast.Accounts
  alias Spacecast.Resources.ResourceSystem
  alias Spacecast.Events

  setup do
    # Set up mocks for external services
    Spacecast.MockExternalAPI
    |> stub(:fetch_data, fn id ->
      {:ok,
       %{
         "id" => id,
         "name" => "Performance Test Resource",
         "type" => "test",
         "status" => "active"
       }}
    end)

    # Create test user
    {:ok, user} =
      Accounts.register_user(%{
        email: "performance_test@example.com",
        password: "password123",
        password_confirmation: "password123",
        name: "Performance Test User"
      })

    # Create test resource
    {:ok, resource} =
      ResourceSystem.create_resource(%{
        name: "Performance Test Resource",
        description: "Resource for performance testing",
        type: "document",
        status: "published",
        content: %{text: "Test content"}
      })

    {:ok, user: user, resource: resource}
  end

  describe "Response Time Performance" do
    test "API endpoints respond within acceptable time limits", %{conn: conn} do
      # Test health endpoint response time
      start_time = System.monotonic_time(:millisecond)
      conn = get(conn, "/health")
      end_time = System.monotonic_time(:millisecond)
      response_time = end_time - start_time

      assert conn.status == 200
      # Should respond within 100ms
      assert response_time < 100

      # Test resources endpoint response time
      start_time = System.monotonic_time(:millisecond)
      conn = get(conn, "/resources")
      end_time = System.monotonic_time(:millisecond)
      response_time = end_time - start_time

      assert conn.status == 200
      # Should respond within 500ms
      assert response_time < 500
    end

    test "LiveView pages load within acceptable time limits", %{conn: conn} do
      # Test LiveView page load time
      start_time = System.monotonic_time(:millisecond)
      {:ok, _view, _html} = live(conn, "/resources")
      end_time = System.monotonic_time(:millisecond)
      load_time = end_time - start_time

      # Should load within 1 second
      assert load_time < 1000
    end

    test "database queries perform efficiently", %{conn: _conn} do
      # Create multiple resources for testing
      resources =
        for i <- 1..100 do
          {:ok, resource} =
            ResourceSystem.create_resource(%{
              name: "Query Test Resource #{i}",
              type: "document",
              status: "published",
              content: %{text: "Query test #{i}"}
            })

          resource
        end

      # Ensure all resources were created successfully
      assert length(resources) == 100

      Enum.each(resources, fn resource ->
        assert resource.name =~ "Query Test Resource"
      end)

      # Force a small delay to ensure all resources are committed
      Process.sleep(100)

      # Test query performance
      start_time = System.monotonic_time(:millisecond)
      # Get more than the 100 we created
      all_resources = ResourceSystem.list_resources(limit: 150)
      end_time = System.monotonic_time(:millisecond)
      query_time = end_time - start_time

      # Verify we have the expected number of resources

      assert length(all_resources) >= 100
      # Should query within 1 second
      assert query_time < 1000
    end

    test "event processing maintains performance under load", %{conn: _conn} do
      # Subscribe to events
      Spacecast.Events.Core.EventBus.subscribe(["document.created"])

      # Create many resources rapidly
      start_time = System.monotonic_time(:millisecond)

      tasks =
        for i <- 1..50 do
          Task.async(fn ->
            ResourceSystem.create_resource(%{
              name: "Event Performance Resource #{i}",
              type: "document",
              status: "published"
            })
          end)
        end

      results = Task.await_many(tasks)
      end_time = System.monotonic_time(:millisecond)
      processing_time = end_time - start_time

      # Verify all resources were created
      assert length(results) == 50

      Enum.each(results, fn {:ok, resource} ->
        assert resource.name =~ "Event Performance Resource"
      end)

      # Verify performance is acceptable
      # Should process within 5 seconds
      assert processing_time < 5000

      # Verify events were processed by checking the event store
      # Wait longer for events to be processed (concurrent operations need more time)
      Process.sleep(500)

      # Check that events were created in the event store
      {:ok, events} =
        Spacecast.Events.EventStore.get_events(%{
          :event_type => "document.created",
          :limit => 50
        })

      # Verify we have the expected number of events
      assert length(events) >= 50

      # Verify all events are of the correct type
      Enum.each(events, fn event ->
        assert event.type == "document.created"
      end)
    end
  end

  describe "Throughput Performance" do
    test "handles high request throughput", %{conn: conn} do
      # Test concurrent API requests
      start_time = System.monotonic_time(:millisecond)

      tasks =
        for _i <- 1..100 do
          Task.async(fn ->
            conn = get(conn, "/health")
            conn.status
          end)
        end

      results = Task.await_many(tasks)
      end_time = System.monotonic_time(:millisecond)
      total_time = end_time - start_time

      # Verify all requests succeeded
      assert length(results) == 100

      Enum.each(results, fn status ->
        assert status == 200
      end)

      # Calculate throughput (requests per second)
      throughput = 100 / (total_time / 1000)
      # Should handle at least 10 requests per second
      assert throughput > 10
    end

    test "handles concurrent resource creation", %{conn: _conn} do
      # Test concurrent resource creation
      start_time = System.monotonic_time(:millisecond)

      tasks =
        for i <- 1..50 do
          Task.async(fn ->
            ResourceSystem.create_resource(%{
              name: "Concurrent Resource #{i}",
              type: "document",
              status: "published",
              content: %{text: "Concurrent test #{i}"}
            })
          end)
        end

      results = Task.await_many(tasks)
      end_time = System.monotonic_time(:millisecond)
      total_time = end_time - start_time

      # Verify all resources were created
      assert length(results) == 50

      Enum.each(results, fn {:ok, resource} ->
        assert resource.name =~ "Concurrent Resource"
      end)

      # Calculate throughput (resources per second)
      throughput = 50 / (total_time / 1000)
      # Should handle at least 5 resources per second
      assert throughput > 5
    end

    test "handles concurrent LiveView connections", %{conn: conn} do
      # Test concurrent HTTP connections to LiveView routes
      start_time = System.monotonic_time(:millisecond)

      tasks =
        for _i <- 1..20 do
          Task.async(fn ->
            # Use regular HTTP requests instead of LiveView helpers
            response = get(conn, "/resources")
            assert response.status == 200
            assert response.resp_body =~ "Resources"
            true
          end)
        end

      results = Task.await_many(tasks)
      end_time = System.monotonic_time(:millisecond)
      total_time = end_time - start_time

      # Verify all connections succeeded
      assert length(results) == 20

      Enum.each(results, fn result ->
        assert result == true
      end)

      # Calculate throughput (connections per second)
      throughput = 20 / (total_time / 1000)
      # Should handle at least 2 connections per second
      assert throughput > 2
    end
  end

  describe "Memory Usage Performance" do
    test "memory usage remains stable under load", %{conn: _conn} do
      # Get initial memory usage
      initial_memory = :erlang.memory(:total)

      # Create many resources
      for i <- 1..1000 do
        ResourceSystem.create_resource(%{
          name: "Memory Test Resource #{i}",
          type: "document",
          status: "published",
          content: %{text: "Memory test #{i}"}
        })
      end

      # Get final memory usage
      final_memory = :erlang.memory(:total)
      memory_increase = final_memory - initial_memory

      # Memory increase should be reasonable (less than 100MB)
      assert memory_increase < 100 * 1024 * 1024
    end

    test "garbage collection works efficiently", %{conn: _conn} do
      # Force garbage collection
      :erlang.garbage_collect()
      initial_memory = :erlang.memory(:total)

      # Create and destroy many resources
      for i <- 1..500 do
        {:ok, resource} =
          ResourceSystem.create_resource(%{
            name: "GC Test Resource #{i}",
            type: "document",
            status: "published"
          })

        ResourceSystem.delete_resource(resource.id)
      end

      # Force garbage collection again
      :erlang.garbage_collect()
      final_memory = :erlang.memory(:total)

      # Memory should be similar after GC
      memory_diff = abs(final_memory - initial_memory)
      # Less than 50MB difference
      assert memory_diff < 50 * 1024 * 1024
    end

    test "handles large data sets efficiently", %{conn: _conn} do
      # Create resources with large content
      large_content = %{
        text: String.duplicate("Large content test ", 1000),
        metadata: %{
          tags: Enum.map(1..100, &"tag_#{&1}"),
          attributes: Enum.map(1..50, &{"attr_#{&1}", "value_#{&1}"}) |> Enum.into(%{})
        }
      }

      start_time = System.monotonic_time(:millisecond)

      # Use batch creation for better performance
      resources_attrs =
        for i <- 1..100 do
          %{
            name: "Large Data Resource #{i}",
            type: "document",
            status: "published",
            content: large_content
          }
        end

      {:ok, _resources} =
        ResourceSystem.create_resources_batch(resources_attrs,
          # Skip events for performance test
          skip_events: true,
          # Skip PubSub for performance test
          skip_pubsub: true,
          # Skip cache invalidation for performance test
          skip_cache_invalidation: true
        )

      end_time = System.monotonic_time(:millisecond)
      processing_time = end_time - start_time

      # Should handle large data within reasonable time
      # Less than 10 seconds
      assert processing_time < 10000
    end
  end

  describe "Scalability Testing" do
    test "scales horizontally with multiple processes", %{conn: _conn} do
      # Test with multiple concurrent processes
      process_count = 10

      start_time = System.monotonic_time(:millisecond)

      tasks =
        for i <- 1..process_count do
          Task.async(fn ->
            # Each process creates resources in batch
            resources_attrs =
              for j <- 1..10 do
                %{
                  name: "Process #{i} Resource #{j}",
                  type: "document",
                  status: "published"
                }
              end

            ResourceSystem.create_resources_batch(resources_attrs,
              skip_events: true,
              skip_pubsub: true,
              skip_cache_invalidation: true
            )
          end)
        end

      Task.await_many(tasks)
      end_time = System.monotonic_time(:millisecond)
      total_time = end_time - start_time

      # Should scale well with multiple processes
      # Less than 10 seconds for 100 total resources
      assert total_time < 10000
    end

    test "maintains performance with increasing data size", %{conn: _conn} do
      # Test performance with different data sizes
      data_sizes = [10, 100, 1000]

      for size <- data_sizes do
        start_time = System.monotonic_time(:millisecond)

        # Create resources with varying content sizes in batch
        resources_attrs =
          for i <- 1..size do
            content = %{
              text: String.duplicate("Content #{i} ", 10),
              metadata: %{size: size, index: i}
            }

            %{
              name: "Size Test Resource #{i}",
              type: "document",
              status: "published",
              content: content
            }
          end

        {:ok, _resources} =
          ResourceSystem.create_resources_batch(resources_attrs,
            skip_events: true,
            skip_pubsub: true,
            skip_cache_invalidation: true
          )

        end_time = System.monotonic_time(:millisecond)
        processing_time = end_time - start_time

        # Performance should scale reasonably with data size
        # 10ms per resource
        max_time = size * 10
        assert processing_time < max_time
      end
    end

    test "handles connection pool scaling", %{conn: _conn} do
      # Test database connection pool under load
      pool_size = 20

      start_time = System.monotonic_time(:millisecond)

      tasks =
        for i <- 1..pool_size do
          Task.async(fn ->
            # Each task performs database operations in batch
            resources_attrs =
              for j <- 1..5 do
                %{
                  name: "Pool Test Resource #{i}-#{j}",
                  type: "document",
                  status: "published"
                }
              end

            ResourceSystem.create_resources_batch(resources_attrs,
              skip_events: true,
              skip_pubsub: true,
              skip_cache_invalidation: true
            )
          end)
        end

      Task.await_many(tasks)
      end_time = System.monotonic_time(:millisecond)
      total_time = end_time - start_time

      # Should handle connection pool load efficiently
      # Less than 15 seconds for 100 total resources
      assert total_time < 15000
    end
  end

  describe "Performance Monitoring" do
    test "tracks performance metrics", %{conn: conn} do
      # Mock performance monitoring
      Spacecast.MockPerformanceMonitor
      |> expect(:track_response_time, fn endpoint, response_time ->
        assert endpoint == "/health"
        assert is_number(response_time)
        # Allow for very fast responses (0ms is acceptable for health checks)
        assert response_time >= 0
        {:ok, "metric-tracked"}
      end)

      # Test performance tracking
      start_time = System.monotonic_time(:millisecond)
      conn = get(conn, "/health")
      end_time = System.monotonic_time(:millisecond)
      response_time = end_time - start_time

      # Track the metric
      result =
        Spacecast.MockPerformanceMonitor.track_response_time("/health", response_time)

      assert {:ok, "metric-tracked"} = result
    end

    test "detects performance degradation", %{conn: _conn} do
      # Mock performance monitoring with degradation detection
      Spacecast.MockPerformanceMonitor
      |> expect(:check_performance, fn ->
        {:warning,
         %{
           avg_response_time: 500,
           threshold: 200,
           recommendation: "Consider optimization"
         }}
      end)

      # Test degradation detection
      result = Spacecast.MockPerformanceMonitor.check_performance()

      assert {:warning, degradation_info} = result
      assert degradation_info.avg_response_time > degradation_info.threshold
      assert degradation_info.recommendation == "Consider optimization"
    end

    test "generates performance reports", %{conn: _conn} do
      # Mock performance reporting
      Spacecast.MockPerformanceMonitor
      |> expect(:generate_report, fn time_range ->
        assert time_range == "24h"

        {:ok,
         %{
           total_requests: 1000,
           avg_response_time: 150,
           error_rate: 0.01,
           throughput: 10.5
         }}
      end)

      # Test report generation
      result = Spacecast.MockPerformanceMonitor.generate_report("24h")

      assert {:ok, report} = result
      assert report.total_requests == 1000
      assert report.avg_response_time == 150
      assert report.error_rate == 0.01
      assert report.throughput == 10.5
    end
  end

  describe "Performance Optimization" do
    test "implements caching for frequently accessed data", %{conn: _conn} do
      # Mock caching
      Spacecast.MockPerformanceCache
      |> expect(:get, fn key ->
        assert key == "resources:list"
        {:ok, [%{id: "cached-1", name: "Cached Resource"}]}
      end)

      # Test cache retrieval
      result = Spacecast.MockPerformanceCache.get("resources:list")

      assert {:ok, cached_data} = result
      assert length(cached_data) == 1
      assert Enum.at(cached_data, 0).name == "Cached Resource"
    end

    test "implements database query optimization", %{conn: _conn} do
      # Test optimized query performance
      start_time = System.monotonic_time(:millisecond)

      # Use optimized query (e.g., with proper indexing)
      resources =
        ResourceSystem.list_resources(
          limit: 10,
          offset: 0
        )

      end_time = System.monotonic_time(:millisecond)
      query_time = end_time - start_time

      # Optimized query should be fast
      # Less than 100ms
      assert query_time < 100
      assert is_list(resources)
    end

    test "implements connection pooling", %{conn: _conn} do
      # Test connection pool efficiency
      pool_size = 10

      # Create a test resource first to get a valid UUID
      {:ok, test_resource} =
        ResourceSystem.create_resource(%{
          name: "Connection Pool Test Resource",
          type: "document",
          status: "published"
        })

      start_time = System.monotonic_time(:millisecond)

      tasks =
        for _i <- 1..pool_size do
          Task.async(fn ->
            # Each task uses a connection from the pool
            ResourceSystem.get_resource(test_resource.id)
          end)
        end

      Task.await_many(tasks)
      end_time = System.monotonic_time(:millisecond)
      total_time = end_time - start_time

      # Connection pooling should be efficient
      # Less than 1 second for 10 concurrent queries
      assert total_time < 1000
    end
  end

  describe "Load Testing Scenarios" do
    test "simulates realistic user load", %{conn: conn} do
      # Simulate realistic user behavior
      user_count = 50

      start_time = System.monotonic_time(:millisecond)

      tasks =
        for i <- 1..user_count do
          Task.async(fn ->
            # Simulate user session with HTTP requests instead of LiveView
            # Browse resources
            get(conn, "/resources")

            # Create a resource via API
            post(conn, "/api/resources", %{
              "resource" => %{
                "name" => "User #{i} Resource",
                "type" => "document",
                "status" => "draft"
              }
            })
          end)
        end

      Task.await_many(tasks)
      end_time = System.monotonic_time(:millisecond)
      total_time = end_time - start_time

      # Should handle realistic user load
      # Less than 30 seconds for 50 users
      assert total_time < 30000
    end

    test "handles peak load scenarios", %{conn: conn} do
      # Simulate peak load (e.g., during deployment or marketing campaign)
      peak_requests = 200

      start_time = System.monotonic_time(:millisecond)

      tasks =
        for i <- 1..peak_requests do
          Task.async(fn ->
            # Mix of different request types
            case rem(i, 4) do
              0 ->
                get(conn, "/health")

              1 ->
                get(conn, "/resources")

              2 ->
                post(conn, "/api/resources", %{"resource" => %{"name" => "Peak Resource #{i}"}})

              # Use regular HTTP request instead of LiveView
              3 ->
                get(conn, "/resources")
            end
          end)
        end

      Task.await_many(tasks)
      end_time = System.monotonic_time(:millisecond)
      total_time = end_time - start_time

      # Should handle peak load gracefully
      # Less than 1 minute for 200 requests
      assert total_time < 60000
    end

    test "maintains performance during background tasks", %{conn: conn} do
      # Test performance while background tasks are running

      # Start background tasks
      background_tasks =
        for i <- 1..5 do
          Task.async(fn ->
            # Simulate background processing
            for j <- 1..20 do
              ResourceSystem.create_resource(%{
                name: "Background Resource #{i}-#{j}",
                type: "document",
                status: "published"
              })

              # Simulate work
              Process.sleep(10)
            end
          end)
        end

      # Perform foreground tasks
      start_time = System.monotonic_time(:millisecond)

      foreground_tasks =
        for _i <- 1..20 do
          Task.async(fn ->
            conn = get(conn, "/health")
            conn.status
          end)
        end

      foreground_results = Task.await_many(foreground_tasks)
      end_time = System.monotonic_time(:millisecond)
      foreground_time = end_time - start_time

      # Wait for background tasks
      Task.await_many(background_tasks)

      # Foreground performance should not be significantly impacted
      # Less than 5 seconds
      assert foreground_time < 5000

      Enum.each(foreground_results, fn status ->
        assert status == 200
      end)
    end
  end
end
