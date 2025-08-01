defmodule SpacecastWeb.Api.ResourceController do
  use SpacecastWeb, :controller

  alias Spacecast.Resources.ResourceSystem
  alias Spacecast.Accounts

  # Plug to ensure authentication
  plug :ensure_authenticated when action in [:create, :update, :delete]

  def index(conn, params) do
    # Parse pagination parameters
    page = String.to_integer(params["page"] || "1")
    per_page = String.to_integer(params["per_page"] || "50")
    # Cap at 100 per page
    limit = min(per_page, 100)
    offset = (page - 1) * limit

    # Parse filters
    filters = %{}
    filters = if params["type"], do: Map.put(filters, :type, params["type"]), else: filters
    filters = if params["status"], do: Map.put(filters, :status, params["status"]), else: filters

    filters =
      if params["parent_id"], do: Map.put(filters, :parent_id, params["parent_id"]), else: filters

    # Get resources with pagination and caching
    opts = [limit: limit, offset: offset, use_cache: true]

    {resources, total_count} =
      if map_size(filters) > 0 do
        {ResourceSystem.list_resources_with_filters(filters, opts),
         ResourceSystem.count_resources(filters)}
      else
        {ResourceSystem.list_resources(opts), ResourceSystem.count_resources()}
      end

    # Calculate pagination metadata
    total_pages = ceil(total_count / limit)
    has_next = page < total_pages
    has_prev = page > 1

    json(conn, %{
      data: Enum.map(resources, &sanitize_resource/1),
      meta: %{
        page: page,
        per_page: limit,
        total_count: total_count,
        total_pages: total_pages,
        has_next: has_next,
        has_prev: has_prev
      }
    })
  end

  def create(conn, %{"resource" => resource_params}) do
    case ResourceSystem.create_resource(resource_params) do
      {:ok, resource} ->
        conn
        |> put_status(:created)
        |> json(%{data: sanitize_resource(resource)})

      {:error, changeset} ->
        conn
        |> put_status(:unprocessable_entity)
        |> json(%{errors: format_changeset_errors(changeset)})
    end
  end

  def show(conn, %{"id" => id}) do
    case validate_uuid(id) do
      :ok ->
        case ResourceSystem.get_resource(id) do
          {:ok, resource} ->
            json(conn, %{data: sanitize_resource(resource)})

          {:error, :not_found} ->
            conn
            |> put_status(:not_found)
            |> json(%{error: "Resource not found"})
        end

      :error ->
        conn
        |> put_status(:bad_request)
        |> json(%{error: "Invalid resource ID format"})
    end
  end

  def update(conn, %{"id" => id, "resource" => resource_params}) do
    case validate_uuid(id) do
      :ok ->
        case ResourceSystem.get_resource(id) do
          {:ok, _resource} ->
            case ResourceSystem.update_resource(id, resource_params) do
              {:ok, updated_resource} ->
                json(conn, %{data: sanitize_resource(updated_resource)})

              {:error, changeset} ->
                conn
                |> put_status(:unprocessable_entity)
                |> json(%{errors: format_changeset_errors(changeset)})
            end

          {:error, :not_found} ->
            conn
            |> put_status(:not_found)
            |> json(%{error: "Resource not found"})
        end

      :error ->
        conn
        |> put_status(:bad_request)
        |> json(%{error: "Invalid resource ID format"})
    end
  end

  def delete(conn, %{"id" => id}) do
    case validate_uuid(id) do
      :ok ->
        case ResourceSystem.get_resource(id) do
          {:ok, _resource} ->
            case ResourceSystem.delete_resource(id) do
              {:ok, _} ->
                conn
                |> put_status(:no_content)
                |> json(%{})

              {:error, _} ->
                conn
                |> put_status(:unprocessable_entity)
                |> json(%{error: "Could not delete resource"})
            end

          {:error, :not_found} ->
            conn
            |> put_status(:not_found)
            |> json(%{error: "Resource not found"})
        end

      :error ->
        conn
        |> put_status(:bad_request)
        |> json(%{error: "Invalid resource ID format"})
    end
  end

  # Private functions

  defp ensure_authenticated(conn, _opts) do
    case get_current_user(conn) do
      nil ->
        conn
        |> put_status(:unauthorized)
        |> json(%{error: "Unauthorized"})
        |> halt()

      _user ->
        conn
    end
  end

  defp get_current_user(conn) do
    # Get user from session or token
    case get_session(conn, :user_token) do
      nil ->
        # Try to get from Authorization header
        case get_req_header(conn, "authorization") do
          ["Bearer " <> token] ->
            Accounts.get_user_by_session_token(token)

          _ ->
            nil
        end

      token ->
        Accounts.get_user_by_session_token(token)
    end
  end

  defp format_changeset_errors(changeset) do
    Ecto.Changeset.traverse_errors(changeset, fn {msg, opts} ->
      Enum.reduce(opts, msg, fn {key, value}, acc ->
        String.replace(acc, "%{#{key}}", to_string(value))
      end)
    end)
  end

  defp sanitize_resource(resource) when is_map(resource) do
    # Convert struct to map first if needed
    resource_map =
      case resource do
        %{__struct__: _} -> Map.from_struct(resource)
        _ -> resource
      end

    # Redact sensitive fields and sanitize strings
    resource_map
    |> Map.take([
      :id,
      :name,
      :type,
      :status,
      :description,
      :content,
      :metadata,
      :settings,
      :version,
      :parent_id,
      :child_ids,
      :tags,
      :categories,
      :created_by,
      :updated_by,
      :inserted_at,
      :updated_at
    ])
    |> Enum.map(fn {k, v} ->
      {k, sanitize_value(v)}
    end)
    |> Enum.into(%{})
  end

  defp sanitize_resource(resource) when is_tuple(resource) do
    # Handle tuple case (e.g., {:ok, resource})
    case resource do
      {:ok, res} -> sanitize_resource(res)
      {_status, res} -> sanitize_resource(res)
      _ -> %{error: "Invalid resource format"}
    end
  end

  defp sanitize_resource(_), do: %{error: "Invalid resource"}

  defp validate_uuid(id) do
    # Simple UUID v4 validation
    uuid_pattern = ~r/^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$/i
    if Regex.match?(uuid_pattern, id), do: :ok, else: :error
  end

  defp sanitize_value(val) when is_binary(val) do
    # Remove <script> tags and redact passwords
    val
    |> String.replace(~r/<script.*?>.*?<\/script>/si, "[removed]", global: true)
    |> String.replace(~r/password\d*/i, "[redacted]", global: true)
    |> String.replace(~r/secret_key_\w+/i, "[redacted]", global: true)
    |> String.replace(~r/private_token_\w+/i, "[redacted]", global: true)
    |> String.replace(~r/credit_card_\d+/i, "[redacted]", global: true)
  end

  defp sanitize_value(%DateTime{} = dt), do: DateTime.to_iso8601(dt)
  defp sanitize_value(%NaiveDateTime{} = dt), do: NaiveDateTime.to_iso8601(dt)

  defp sanitize_value(val) when is_map(val),
    do: Enum.into(val, %{}, fn {k, v} -> {k, sanitize_value(v)} end)

  defp sanitize_value(val) when is_list(val), do: Enum.map(val, &sanitize_value/1)
  defp sanitize_value(val), do: val
end
