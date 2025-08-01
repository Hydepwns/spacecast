defmodule SpacecastWeb.Api.UploadController do
  use SpacecastWeb, :controller

  alias Spacecast.Accounts

  # Plug to ensure authentication
  plug :ensure_authenticated when action in [:create]

  def create(conn, %{"file" => upload_params}) do
    # Validate file upload
    case validate_upload(upload_params) do
      {:ok, file_data} ->
        # Process the upload (for now, just return success)
        upload_result = %{
          id: generate_upload_id(),
          filename: file_data.filename,
          size: file_data.size,
          content_type: file_data.content_type,
          uploaded_at: DateTime.utc_now()
        }

        conn
        |> put_status(:created)
        |> json(%{data: sanitize_upload_result(upload_result)})

      {:error, reason} ->
        conn
        |> put_status(:bad_request)
        |> json(%{error: reason})
    end
  end

  def create(conn, _params) do
    conn
    |> put_status(:bad_request)
    |> json(%{error: "No file provided"})
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

  defp validate_upload(upload_params) do
    # Check if file exists
    case upload_params do
      %Plug.Upload{filename: filename, content_type: content_type} ->
        # Get file size from the file
        file_size = File.stat!(upload_params.path).size

        # Validate file size (max 10MB)
        if file_size > 10 * 1024 * 1024 do
          {:error, "File too large. Maximum size is 10MB."}
        else
          # Validate content type
          allowed_types = [
            "image/jpeg",
            "image/png",
            "image/gif",
            "application/pdf",
            "text/plain",
            "application/json"
          ]

          if content_type in allowed_types do
            {:ok, %{filename: filename, content_type: content_type, size: file_size}}
          else
            {:error, "Invalid file type. Allowed types: #{Enum.join(allowed_types, ", ")}"}
          end
        end

      _ ->
        {:error, "Invalid file upload"}
    end
  end

  defp generate_upload_id do
    :crypto.strong_rand_bytes(16)
    |> Base.encode16(case: :lower)
  end

  defp sanitize_upload_result(upload_result) do
    Map.take(upload_result, [:id, :filename, :size, :content_type, :uploaded_at])
  end
end
