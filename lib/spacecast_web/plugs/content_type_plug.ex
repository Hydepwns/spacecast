defmodule SpacecastWeb.Plugs.ContentTypePlug do
  @moduledoc """
  Content-Type validation plug for ensuring proper media type headers on requests.
  """

  import Plug.Conn
  import Phoenix.Controller

  def init(opts), do: opts

  def call(conn, _opts) do
    # Only check content-type for POST/PUT/PATCH requests with actual body content
    if method_requires_content_type?(conn.method) and has_body_content?(conn) do
      validate_content_type(conn)
    else
      conn
    end
  end

  defp validate_content_type(conn) do
    case get_req_header(conn, "content-type") do
      [content_type | _] ->
        if valid_content_type?(content_type) do
          conn
        else
          conn
          |> put_status(:unsupported_media_type)
          |> json(%{error: "Unsupported media type"})
          |> halt()
        end

      [] ->
        conn
        |> put_status(:unsupported_media_type)
        |> json(%{error: "Content-Type header required"})
        |> halt()
    end
  end

  defp method_requires_content_type?("POST"), do: true
  defp method_requires_content_type?("PUT"), do: true
  defp method_requires_content_type?("PATCH"), do: true
  defp method_requires_content_type?(_), do: false

  defp has_body_content?(conn) do
    # Check if there's actual body content
    case conn.body_params do
      %{} when map_size(conn.body_params) > 0 ->
        true

      _ ->
        # Also check if there's a content-type header indicating body content
        case get_req_header(conn, "content-type") do
          [_ | _] -> true
          [] -> false
        end
    end
  end

  defp valid_content_type?(content_type) do
    content_type
    |> String.downcase()
    |> String.contains?("application/json")
  end
end
