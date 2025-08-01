defmodule Spacecast.Notifications.EmailAdapter do
  @moduledoc """
  Email notification adapter for sending emails through various providers.

  This module provides a unified interface for sending emails regardless of the
  underlying email service provider.
  """

  @behaviour Spacecast.Notifications.EmailAdapterBehaviour

  require Logger

  @doc """
  Sends an email to the specified recipient.

  ## Parameters
  - to: Email address of the recipient
  - subject: Subject line of the email
  - body: Body content of the email

  ## Returns
  - {:ok, message_id} on success
  - {:error, reason} on failure
  """
  @impl true
  def send_email(to, subject, body) do
    # Default implementation - in production this would integrate with
    # a real email service like SendGrid, Mailgun, etc.
    case validate_email_params(to, subject, body) do
      :ok ->
        # Mock successful email sending
        message_id = generate_message_id()
        log_email_sent(to, subject, message_id)
        {:ok, message_id}

      {:error, reason} ->
        {:error, reason}
    end
  end

  @doc """
  Sends an email with HTML content.

  ## Parameters
  - to: Email address of the recipient
  - subject: Subject line of the email
  - html_body: HTML content of the email
  - text_body: Plain text fallback content (optional)

  ## Returns
  - {:ok, message_id} on success
  - {:error, reason} on failure
  """
  @impl true
  def send_html_email(to, subject, html_body, _text_body \\ nil) do
    # For now, delegate to the regular send_email function
    # In a real implementation, this would handle HTML emails differently
    send_email(to, subject, html_body)
  end

  @doc """
  Sends a templated email using a predefined template.

  ## Parameters
  - to: Email address of the recipient
  - template_name: Name of the template to use
  - template_data: Data to inject into the template

  ## Returns
  - {:ok, message_id} on success
  - {:error, reason} on failure
  """
  @impl true
  def send_templated_email(to, template_name, template_data) do
    # Mock template rendering
    subject = "Templated Email: #{template_name}"
    body = render_template(template_name, template_data)

    send_email(to, subject, body)
  end

  # Private helper functions

  defp validate_email_params(to, subject, body) do
    cond do
      is_nil(to) or to == "" ->
        {:error, "Recipient email is required"}

      not is_valid_email(to) ->
        {:error, "Invalid email format"}

      is_nil(subject) or subject == "" ->
        {:error, "Email subject is required"}

      is_nil(body) or body == "" ->
        {:error, "Email body is required"}

      true ->
        :ok
    end
  end

  defp is_valid_email(email) do
    # Simple email validation - in production, use a proper email validation library
    String.contains?(email, "@") and String.contains?(email, ".")
  end

  defp generate_message_id do
    # Generate a unique message ID
    "email-#{System.system_time(:millisecond)}-#{random_string(8)}"
  end

  defp random_string(length) do
    :crypto.strong_rand_bytes(length)
    |> Base.url_encode64()
    |> binary_part(0, length)
  end

  defp log_email_sent(to, subject, message_id) do
    # Log email sending for debugging/monitoring
    Logger.info("Email sent: #{message_id} to #{to} - #{subject}")
  end

  defp render_template(template_name, template_data) do
    # Mock template rendering
    # In production, this would use a proper templating engine
    """
    Template: #{template_name}
    Data: #{inspect(template_data)}

    This is a mock template email.
    """
  end
end
