defmodule Spacecast.Events.Adapters.EmailAdapter do
  @moduledoc """
  Adapter for sending encrypted email reminders.
  Supports multiple email providers including SendGrid, SMTP, and custom providers.
  """

  @behaviour Spacecast.Events.Adapters.Adapter
  require Logger

  @impl true
  def send_reminder(reminder, _settings, config, encrypted_message) do
    with {:ok, reminder} <- validate_reminder(reminder),
         {:ok, email} <- validate_email(Map.get(reminder, :recipient)),
         {:ok, email_content} <- build_email_content(reminder, encrypted_message, config),
         {:ok, _response} <- send_email(email, email_content, config) do
      Logger.info("Email sent successfully to #{email}")
      {:ok, "Email sent successfully"}
    else
      {:error, reason} -> {:error, reason}
    end
  end

  # Validate that reminder is not nil or empty
  defp validate_reminder(nil), do: {:error, "Missing reminder"}

  defp validate_reminder(reminder) when is_map(reminder) do
    case map_size(reminder) do
      0 -> {:error, "Empty reminder"}
      _ -> {:ok, reminder}
    end
  end

  defp validate_reminder(_), do: {:error, "Invalid reminder format"}

  # Validate email format
  defp validate_email(nil), do: {:error, "Invalid email address"}
  defp validate_email(""), do: {:error, "Invalid email address"}

  defp validate_email(email) when is_binary(email) do
    case Regex.run(~r/^[^\s@]+@[^\s@]+\.[^\s@]+$/, email) do
      [_] -> {:ok, email}
      nil -> {:error, "Invalid email address"}
    end
  end

  defp validate_email(_), do: {:error, "Invalid email address"}

  defp build_email_content(reminder, encrypted_message, config) do
    # Build email content with both plain text and HTML versions
    content = %{
      subject: Map.get(reminder, :title, "Event Reminder"),
      text: build_plain_text_content(reminder, encrypted_message, config),
      html: build_html_content(reminder, encrypted_message, config),
      attachments: build_attachments(reminder, config)
    }

    {:ok, content}
  end

  defp build_plain_text_content(reminder, encrypted_message, config) do
    """
    Hello,

    #{Map.get(reminder, :message, "You have a reminder")}

    Encrypted Message:
    #{encrypted_message}

    Reference: #{Map.get(reminder, :id, "N/A")}
    Sent at: #{DateTime.utc_now() |> DateTime.to_iso8601()}

    Best regards,
    #{Map.get(config, :from_name, "Hydepwns Liveview")}
    """
  end

  defp build_html_content(reminder, encrypted_message, config) do
    """
    <!DOCTYPE html>
    <html>
      <head>
        <meta charset=\"utf-8\">
        <style>
          body { font-family: Arial, sans-serif; line-height: 1.6; color: #333; }
          .container { max-width: 600px; margin: 0 auto; padding: 20px; }
          .message { margin: 20px 0; }
          .encrypted { background: #f5f5f5; padding: 15px; border-radius: 5px; margin: 20px 0; }
          .footer { margin-top: 30px; font-size: 0.9em; color: #666; }
        </style>
      </head>
      <body>
        <div class=\"container\">
          <h2>#{Map.get(reminder, :title, "Event Reminder")}</h2>
          <div class=\"message\">
            #{Map.get(reminder, :message, "You have a reminder")}
          </div>
          <div class=\"encrypted\">
            <strong>Encrypted Message:</strong><br>
            #{encrypted_message}
          </div>
          <div class=\"footer\">
            <p>Reference: #{Map.get(reminder, :id, "N/A")}</p>
            <p>Sent at: #{DateTime.utc_now() |> DateTime.to_iso8601()}</p>
            <p>Best regards,<br>#{Map.get(config, :from_name, "Hydepwns Liveview")}</p>
          </div>
        </div>
      </body>
    </html>
    """
  end

  defp build_attachments(reminder, config) do
    attachments =
      case Map.get(config, :attachments, []) do
        attachments when is_list(attachments) ->
          Enum.map(attachments, fn attachment ->
            %{
              filename: attachment.filename,
              content: attachment.content,
              type: attachment.type
            }
          end)

        _ ->
          []
      end

    # Add reminder-specific attachments if any
    reminder_attachments =
      case Map.get(reminder, :attachments, []) do
        attachments when is_list(attachments) -> attachments
        _ -> []
      end

    attachments ++ reminder_attachments
  end

  defp send_email(email, content, config) do
    case Map.get(config, :provider) do
      "sendgrid" ->
        send_sendgrid_email(email, content.subject, content.text, Map.get(config, :api_key, ""))

      "smtp" ->
        send_smtp_email(email, content, config)

      "custom" ->
        send_custom_email(email, content, config)

      _ ->
        {:error, "Unsupported email provider"}
    end
  end

  defp send_sendgrid_email(to, subject, body, api_key) do
    url = "https://api.sendgrid.com/v3/mail/send"

    headers = [
      {"Authorization", "Bearer #{api_key}"},
      {"Content-Type", "application/json"}
    ]

    body =
      Jason.encode!(%{
        personalizations: [%{to: [%{email: to}]}],
        from: %{email: "noreply@hydepwns.com"},
        subject: subject,
        content: [%{type: "text/plain", value: body}]
      })

    case HTTPoison.post(url, body, headers) do
      {:ok, %{status_code: status_code}} when status_code in 200..299 ->
        {:ok, "Email sent successfully"}

      {:ok, %{status_code: status_code}} ->
        {:error, "Failed to send email with status code: #{status_code}"}

      {:error, %HTTPoison.Error{reason: reason}} ->
        {:error, "Failed to send email: #{reason}"}
    end
  end

  defp send_smtp_email(email, content, config) do
    try do
      # Configure SMTP settings
      smtp_config = [
        relay: Map.get(config, :smtp_relay),
        port: Map.get(config, :smtp_port),
        username: Map.get(config, :smtp_username),
        password: Map.get(config, :smtp_password),
        ssl: Map.get(config, :smtp_ssl, false),
        tls: Map.get(config, :smtp_tls, :always),
        auth: Map.get(config, :smtp_auth, :always)
      ]

      # Create email message
      message = %Swoosh.Email{
        to: email,
        from:
          {Map.get(config, :from_name, "Hydepwns Liveview"),
           Map.get(config, :from_email, "noreply@hydepwns.com")},
        subject: content.subject,
        text_body: content.text,
        html_body: content.html,
        attachments: content.attachments
      }

      # Send email using Swoosh
      case Swoosh.Mailer.deliver(message, smtp_config) do
        {:ok, _response} ->
          {:ok, %{message_id: Ecto.UUID.generate()}}

        {:error, reason} ->
          Logger.error("SMTP error: #{inspect(reason)}")
          {:error, "Failed to send email via SMTP"}
      end
    rescue
      e ->
        Logger.error("SMTP error: #{inspect(e)}")
        {:error, "SMTP service error"}
    end
  end

  defp send_custom_email(email, content, config) do
    try do
      # Call custom email provider function
      case Map.get(config, :custom_provider, %{
             send_email: fn _, _, _ -> {:error, "Custom provider error"} end
           }).send_email.(email, content, config) do
        {:ok, response} ->
          {:ok, %{message_id: response.message_id}}

        {:error, reason} ->
          Logger.error("Custom provider error: #{inspect(reason)}")
          {:error, "Failed to send email via custom provider"}
      end
    rescue
      e ->
        Logger.error("Custom provider error: #{inspect(e)}")
        {:error, "Custom provider service error"}
    end
  end
end
