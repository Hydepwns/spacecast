defmodule SpacecastWeb.Integration.APIIntegrationTest do
  @moduledoc """
  Comprehensive API integration tests covering all JSON endpoints,
  authentication, error handling, and response validation.
  """

  use SpacecastWeb.ConnCase, async: false
  import Mox
  setup :set_mox_from_context
  setup :verify_on_exit!

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
  alias Spacecast.ThemeSystem

  setup do
    # Set up mocks for external services
    Spacecast.MockExternalAPI
    |> stub(:fetch_data, fn id ->
      {:ok,
       %{
         "id" => id,
         "name" => "Test Resource",
         "description" => "A test resource",
         "type" => "test-type",
         "status" => "active"
       }}
    end)

    # Create test user with unique email
    unique_email = "api_test_#{System.system_time(:millisecond)}@example.com"

    {:ok, user} =
      Accounts.register_user(%{
        email: unique_email,
        password: "password123",
        password_confirmation: "password123",
        name: "API Test User"
      })

    # Create test resource with unique name
    unique_resource_name = "API Test Resource #{System.system_time(:millisecond)}"

    {:ok, resource} =
      ResourceSystem.create_resource(%{
        name: unique_resource_name,
        description: "Resource for API testing",
        type: "document",
        status: "published",
        content: %{text: "Test content"}
      })

    # Create test theme with unique name to avoid constraint violations
    unique_theme_name = "API Test Theme #{System.system_time(:millisecond)}"

    {:ok, theme} =
      ThemeSystem.create_theme(%{
        name: unique_theme_name,
        mode: "light",
        primary_color: "#3b82f6",
        secondary_color: "#6b7280"
      })

    {:ok, user: user, resource: resource, theme: theme}
  end

  describe "Health Check API" do
    test "GET /health returns basic health status", %{conn: conn} do
      conn = get(conn, "/health")

      assert conn.status == 200
      response = json_response(conn, 200)

      assert response["status"] == "healthy"
      assert is_binary(response["timestamp"])
      assert is_binary(response["version"])
    end

    test "GET /health/detailed returns comprehensive health status", %{conn: conn} do
      conn = get(conn, "/health/detailed")

      assert conn.status == 200
      response = json_response(conn, 200)

      assert response["status"] in ["healthy", "degraded"]
      assert is_map(response["checks"])
      assert is_map(response["checks"]["database"])
      assert is_map(response["checks"]["memory"])
      assert is_map(response["checks"]["disk"])
    end

    test "GET /health/ready returns readiness status", %{conn: conn} do
      conn = get(conn, "/health/ready")

      assert conn.status == 200
      response = json_response(conn, 200)

      assert response["ready"] == true
      assert is_binary(response["timestamp"])
    end

    test "GET /health/live returns liveness status", %{conn: conn} do
      conn = get(conn, "/health/live")

      assert conn.status == 200
      response = json_response(conn, 200)

      assert response["alive"] == true
      assert is_binary(response["timestamp"])
    end
  end

  describe "Theme API" do
    # test "GET /themes returns list of themes", %{conn: conn} do
    #   conn = get(conn, "/themes")

    #   assert conn.status == 200
    #   response = json_response(conn, 200)

    #   assert is_map(response["data"])
    #   assert is_list(response["data"])
    #   assert length(response["data"]) > 0

    #   theme = List.first(response["data"])
    #   assert is_binary(theme["id"])
    #   assert is_binary(theme["name"])
    #   assert is_binary(theme["mode"])
    # end

    # test "GET /themes/:id returns single theme", %{conn: conn, theme: theme} do
    #   conn = get(conn, "/themes/#{theme.id}")

    #   assert conn.status == 200
    #   response = json_response(conn, 200)

    #   assert is_map(response["data"])
    #   theme_data = response["data"]
    #   assert theme_data["id"] == theme.id
    #   assert theme_data["name"] == theme.name
    #   assert theme_data["mode"] == theme.mode
    # end

    # test "POST /themes creates new theme", %{conn: conn} do
    #   theme_params = %{
    #     "theme" => %{
    #       "name" => "API Created Theme",
    #       "mode" => "dark",
    #       "primary_color" => "#1f2937",
    #       "secondary_color" => "#9ca3af"
    #     }
    #   }

    #   conn = post(conn, "/themes", theme_params)

    #   assert conn.status == 201
    #   response = json_response(conn, 201)

    #   assert is_map(response["data"])
    #   theme_data = response["data"]
    #   assert theme_data["name"] == "API Created Theme"
    #   assert theme_data["mode"] == "dark"
    # end

    # test "PUT /themes/:id updates existing theme", %{conn: conn, theme: theme} do
    #   update_params = %{
    #     "theme" => %{
    #       "name" => "Updated Theme Name",
    #       "primary_color" => "#ef4444"
    #     }
    #   }

    #   conn = put(conn, "/themes/#{theme.id}", update_params)

    #   assert conn.status == 200
    #   response = json_response(conn, 200)

    #   assert is_map(response["data"])
    #   theme_data = response["data"]
    #   assert theme_data["name"] == "Updated Theme Name"
    #   assert theme_data["primary_color"] == "#ef4444"
    # end

    # test "DELETE /themes/:id deletes theme", %{conn: conn, theme: theme} do
    #   conn = delete(conn, "/themes/#{theme.id}")

    #   assert conn.status == 204

    #   # Verify theme is deleted
    #   conn = get(conn, "/themes/#{theme.id}")
    #   assert conn.status == 404
    # end
  end

  describe "Resource API" do
    test "GET /api/resources returns list of resources", %{conn: conn} do
      conn = get(conn, "/api/resources")

      assert conn.status == 200
      response = json_response(conn, 200)

      assert is_list(response["data"])
      assert length(response["data"]) > 0

      resource = List.first(response["data"])
      assert is_binary(resource["id"])
      assert is_binary(resource["name"])
      assert is_binary(resource["type"])
    end

    test "GET /api/resources/:id returns single resource", %{conn: conn, resource: resource} do
      conn = get(conn, "/api/resources/#{resource.id}")

      assert conn.status == 200
      response = json_response(conn, 200)

      assert is_map(response["data"])
      resource_data = response["data"]
      assert resource_data["id"] == resource.id
      assert resource_data["name"] == resource.name
      assert resource_data["type"] == resource.type
    end

    test "POST /api/resources creates new resource", %{conn: conn, user: user} do
      resource_params = %{
        "resource" => %{
          "name" => "API Created Resource",
          "description" => "Created via API",
          "type" => "document",
          "status" => "draft",
          "content" => %{"text" => "API content"}
        }
      }

      # Add authentication and proper Content-Type
      token = Accounts.generate_user_session_token(user)

      conn =
        conn
        |> put_req_header("authorization", "Bearer #{token}")
        |> put_req_header("content-type", "application/json")

      conn = post(conn, "/api/resources", resource_params)

      assert conn.status == 201
      response = json_response(conn, 201)

      assert is_map(response["data"])
      resource_data = response["data"]
      assert resource_data["name"] == "API Created Resource"
      assert resource_data["type"] == "document"
    end

    test "PUT /api/resources/:id updates existing resource", %{
      conn: conn,
      resource: resource,
      user: user
    } do
      update_params = %{
        "resource" => %{
          "name" => "Updated Resource Name",
          "status" => "published"
        }
      }

      # Add authentication and proper Content-Type
      token = Accounts.generate_user_session_token(user)

      conn =
        conn
        |> put_req_header("authorization", "Bearer #{token}")
        |> put_req_header("content-type", "application/json")

      conn = put(conn, "/api/resources/#{resource.id}", update_params)

      assert conn.status == 200
      response = json_response(conn, 200)

      assert is_map(response["data"])
      resource_data = response["data"]
      assert resource_data["name"] == "Updated Resource Name"
      assert resource_data["status"] == "published"
    end

    test "DELETE /api/resources/:id deletes resource", %{
      conn: conn,
      resource: resource,
      user: user
    } do
      # Add authentication
      token = Accounts.generate_user_session_token(user)
      conn = conn |> put_req_header("authorization", "Bearer #{token}")

      conn = delete(conn, "/api/resources/#{resource.id}")

      assert conn.status == 204

      # Verify resource is deleted
      conn = get(conn, "/api/resources/#{resource.id}")
      assert conn.status == 404
    end
  end

  # Event API - Not implemented in current API
  # describe "Event API" do
  #   test "GET /api/events returns list of events", %{conn: conn} do
  #     # TODO: Implement public events API endpoint
  #   end
  #
  #   test "GET /api/events/:id returns single event", %{conn: conn} do
  #     # TODO: Implement public event detail API endpoint
  #   end
  #
  #   test "POST /api/events creates new event", %{conn: conn} do
  #     # TODO: Implement public event creation API endpoint
  #   end
  # end

  describe "Authentication & Authorization" do
    test "protected endpoints require authentication", %{conn: conn} do
      # Try to access protected endpoint without authentication
      conn = get(conn, "/api/admin/users")
      assert conn.status == 401

      response = json_response(conn, 401)
      assert response["error"] == "Unauthorized"
    end

    test "admin endpoints require admin role", %{conn: conn, user: user} do
      # Login as regular user
      token = Accounts.generate_user_session_token(user)
      conn = conn |> put_req_header("authorization", "Bearer #{token}")

      # Try to access admin endpoint
      conn = get(conn, "/api/admin/users")
      assert conn.status == 403

      response = json_response(conn, 403)
      assert response["error"] == "Forbidden"
    end

    test "valid token allows access to protected endpoints", %{conn: conn, user: user} do
      # Login as user
      token = Accounts.generate_user_session_token(user)
      conn = conn |> put_req_header("authorization", "Bearer #{token}")

      # Access protected endpoint
      conn = get(conn, "/api/users/profile")
      assert conn.status == 200
    end
  end

  describe "Error Handling" do
    test "returns 404 for non-existent resources", %{conn: conn} do
      conn = get(conn, "/api/resources/00000000-0000-0000-0000-000000000000")
      assert conn.status == 404

      response = json_response(conn, 404)
      assert response["error"] == "Resource not found"
    end

    test "returns 422 for validation errors", %{conn: conn, user: user} do
      invalid_params = %{
        "resource" => %{
          # Invalid: empty name
          "name" => "",
          # Invalid: unknown type
          "type" => "invalid_type"
        }
      }

      # Add authentication and proper Content-Type
      token = Accounts.generate_user_session_token(user)

      conn =
        conn
        |> put_req_header("authorization", "Bearer #{token}")
        |> put_req_header("content-type", "application/json")

      conn = post(conn, "/api/resources", invalid_params)
      assert conn.status == 422

      response = json_response(conn, 422)
      assert is_map(response["errors"])
      assert response["errors"]["name"] == ["can't be blank"]
    end

    # test "returns 500 for server errors", %{conn: conn} do
    #   # TODO: Implement external API endpoint for testing server errors
    #   # Mock external API to return error
    #   Spacecast.MockExternalAPI
    #   |> expect(:fetch_data, fn _id -> {:error, "Service unavailable"} end)
    #
    #   conn = get(conn, "/api/external/resource/123")
    #   assert conn.status == 500
    #
    #   response = json_response(conn, 500)
    #   assert response["error"] == "Internal Server Error"
    # end
  end

  # describe "Rate Limiting" do
  #   test "enforces rate limits on API endpoints", %{conn: conn} do
  #     # Make multiple rapid requests
  #     responses = for _ <- 1..10 do
  #       conn = get(conn, "/api/resources")
  #       conn.status
  #     end

  #     # Should eventually hit rate limit
  #     assert Enum.any?(responses, &(&1 == 429))
  #   end
  # end

  describe "Response Format" do
    # test "all responses follow consistent JSON structure", %{conn: conn} do
    #   conn = get(conn, "/api/resources")

    #   assert conn.status == 200
    #   response = json_response(conn, 200)

    #   # Check for consistent structure
    #   assert Map.has_key?(response, "data")
    #   assert Map.has_key?(response, "meta")
    #   assert is_map(response["meta"])
    #   assert Map.has_key?(response["meta"], "timestamp")
    # end

    test "error responses follow consistent format", %{conn: conn} do
      conn = get(conn, "/api/resources/00000000-0000-0000-0000-000000000000")

      assert conn.status == 404
      response = json_response(conn, 404)

      # Check for consistent error structure
      assert Map.has_key?(response, "error")
      assert is_binary(response["error"])
    end
  end

  # describe "Pagination" do
  #   test "supports pagination parameters", %{conn: conn} do
  #     # Create multiple resources for pagination testing
  #     for i <- 1..15 do
  #       ResourceSystem.create_resource(%{
  #         name: "Pagination Resource #{i}",
  #         type: "document",
  #         status: "published",
  #         content: %{text: "Content #{i}"}
  #       })
  #     end

  #     # Test first page
  #     conn = get(conn, "/api/resources?page=1&per_page=10")
  #     assert conn.status == 200
  #     response = json_response(conn, 200)

  #     assert length(response["data"]) == 10
  #     assert response["meta"]["page"] == 1
  #     assert response["meta"]["per_page"] == 10
  #     assert response["meta"]["total_pages"] > 1

  #     # Test second page
  #     conn = get(conn, "/api/resources?page=2&per_page=10")
  #     assert conn.status == 200
  #     response = json_response(conn, 200)

  #     assert length(response["data"]) > 0
  #     assert response["meta"]["page"] == 2
  #   end
  # end

  describe "Filtering & Sorting" do
    test "supports filtering by resource type", %{conn: conn} do
      conn = get(conn, "/api/resources?type=document")
      assert conn.status == 200
      response = json_response(conn, 200)

      # All returned resources should be documents
      Enum.each(response["data"], fn resource ->
        assert resource["type"] == "document"
      end)
    end

    test "supports sorting by name", %{conn: conn} do
      conn = get(conn, "/api/resources?sort=name&order=asc")
      assert conn.status == 200
      response = json_response(conn, 200)

      # Verify resources are sorted by name
      names = Enum.map(response["data"], & &1["name"])
      assert names == Enum.sort(names)
    end
  end

  # Bulk Operations - Not implemented in current API
  # describe "Bulk Operations" do
  #   test "supports bulk resource creation", %{conn: conn} do
  #     # TODO: Implement bulk resource creation endpoint
  #   end
  #
  #   test "supports bulk resource updates", %{conn: conn} do
  #     # TODO: Implement bulk resource update endpoint
  #   end
  # end
end
