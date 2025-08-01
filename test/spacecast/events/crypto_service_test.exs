defmodule Spacecast.Events.CryptoServiceTest do
  use ExUnit.Case, async: true
  alias Spacecast.Events.CryptoService

  setup do
    # Initialize the crypto service
    CryptoService.init()
    :ok
  end

  describe "initialization" do
    test "initializes session storage" do
      assert :ok = CryptoService.init()
    end
  end

  describe "session management" do
    test "creates a new session" do
      assert {:ok, session_id} = CryptoService.create_session("user1", "user2")
      assert is_binary(session_id)
      assert String.length(session_id) > 0
    end

    test "retrieves a session by ID" do
      {:ok, session_id} = CryptoService.create_session("user1", "user2")
      assert {:ok, session} = CryptoService.get_session(session_id)
      assert session.session_id == session_id
      assert session.local_user_id == "user1"
      assert session.remote_user_id == "user2"
    end

    test "returns error for non-existent session" do
      assert {:error, :not_found} = CryptoService.get_session("non-existent")
    end

    test "lists all sessions" do
      {:ok, session1} = CryptoService.create_session("user1", "user2")
      {:ok, session2} = CryptoService.create_session("user3", "user4")

      sessions = CryptoService.list_sessions()
      assert length(sessions) >= 2
      assert Enum.any?(sessions, fn s -> s.session_id == session1 end)
      assert Enum.any?(sessions, fn s -> s.session_id == session2 end)
    end

    test "deletes a session" do
      {:ok, session_id} = CryptoService.create_session("user1", "user2")
      assert :ok = CryptoService.delete_session(session_id)
      assert {:error, :not_found} = CryptoService.get_session(session_id)
    end

    test "returns error when deleting non-existent session" do
      assert {:error, :not_found} = CryptoService.delete_session("non-existent")
    end
  end

  describe "message encryption and decryption" do
    test "encrypts and decrypts a message" do
      reminder = %{message: "Hello, encrypted world!"}
      settings = %{recipient_id: "user123"}

      # Encrypt the message
      assert {:ok, encrypted} = CryptoService.encrypt_message(reminder, settings)
      assert is_binary(encrypted)
      assert encrypted != reminder.message

      # Get the session to decrypt
      sessions = CryptoService.list_sessions()
      session = Enum.find(sessions, fn s -> s.remote_user_id == "user123" end)
      assert session != nil

      # Decrypt the message
      assert {:ok, decrypted} = CryptoService.decrypt_message(encrypted, session.session_id)
      assert decrypted == reminder.message
    end

    test "encrypts and decrypts with session directly" do
      {:ok, session_id} = CryptoService.create_session("user1", "user2")
      {:ok, session} = CryptoService.get_session(session_id)

      message = "Direct session encryption test"

      # Encrypt with session
      assert {:ok, encrypted} = CryptoService.encrypt_with_session(session, message)
      assert is_binary(encrypted)

      # Decrypt with session
      assert {:ok, decrypted} = CryptoService.decrypt_with_session(session, encrypted)
      assert decrypted == message
    end

    test "handles multiple messages in same session" do
      {:ok, session_id} = CryptoService.create_session("user1", "user2")
      {:ok, session} = CryptoService.get_session(session_id)

      # Send one message and check if session is updated
      assert {:ok, encrypted} = CryptoService.encrypt_with_session(session, "Message 1")
      {:ok, updated_session} = CryptoService.get_session(session_id)

      # Check if message count increased
      assert updated_session.message_count == 1

      # Send another message
      assert {:ok, encrypted2} = CryptoService.encrypt_with_session(updated_session, "Message 2")
      {:ok, final_session} = CryptoService.get_session(session_id)

      # Check if message count increased again
      assert final_session.message_count == 2
    end

    test "returns error for invalid encrypted message" do
      {:ok, session_id} = CryptoService.create_session("user1", "user2")
      {:ok, session} = CryptoService.get_session(session_id)

      assert {:error, _} = CryptoService.decrypt_with_session(session, "invalid-base64")
    end
  end

  describe "digital signatures" do
    test "signs and verifies data" do
      data = "Important message to sign"
      key = :crypto.strong_rand_bytes(32)

      # Sign the data
      assert {:ok, signature} = CryptoService.sign_data(key, data)
      assert is_binary(signature)

      # Verify the signature
      assert {:ok, true} = CryptoService.verify_signature(key, data, signature)
    end

    test "rejects invalid signatures" do
      data = "Important message to sign"
      key = :crypto.strong_rand_bytes(32)
      wrong_key = :crypto.strong_rand_bytes(32)

      # Sign with one key
      {:ok, signature} = CryptoService.sign_data(key, data)

      # Try to verify with wrong key
      assert {:ok, false} = CryptoService.verify_signature(wrong_key, data, signature)
    end

    test "rejects tampered data" do
      data = "Important message to sign"
      tampered_data = "Tampered message to sign"
      key = :crypto.strong_rand_bytes(32)

      # Sign the original data
      {:ok, signature} = CryptoService.sign_data(key, data)

      # Try to verify with tampered data
      assert {:ok, false} = CryptoService.verify_signature(key, tampered_data, signature)
    end
  end

  describe "session statistics" do
    test "provides session statistics" do
      # Create some sessions
      {:ok, _} = CryptoService.create_session("user1", "user2")
      {:ok, _} = CryptoService.create_session("user3", "user4")

      # Send some messages
      reminder1 = %{message: "Test message 1"}
      reminder2 = %{message: "Test message 2"}
      settings = %{recipient_id: "user2"}

      {:ok, _} = CryptoService.encrypt_message(reminder1, settings)
      {:ok, _} = CryptoService.encrypt_message(reminder2, settings)

      # Get statistics
      stats = CryptoService.get_session_stats()

      assert is_map(stats)
      assert Map.has_key?(stats, :total_sessions)
      assert Map.has_key?(stats, :total_messages)
      assert Map.has_key?(stats, :oldest_session)
      assert Map.has_key?(stats, :newest_session)
      assert stats.total_sessions >= 2
      assert stats.total_messages >= 2
    end
  end

  describe "session cleanup" do
    test "cleans up expired sessions" do
      # Create a session
      {:ok, session_id} = CryptoService.create_session("user1", "user2")

      # Manually set the session to be old
      session = CryptoService.get_session(session_id) |> elem(1)
      old_session = Map.put(session, :created_at, DateTime.add(DateTime.utc_now(), -25 * 60 * 60, :second))
      :ets.insert(:signal_sessions, {session_id, old_session})

      # Clean up expired sessions
      cleaned_count = CryptoService.cleanup_expired_sessions()
      assert cleaned_count >= 1

      # Verify session was deleted
      assert {:error, :not_found} = CryptoService.get_session(session_id)
    end
  end

  describe "error handling" do
    test "handles encryption errors gracefully" do
      # Try to encrypt with invalid session
      invalid_session = %{session_id: "invalid", protocol: :invalid}

      assert {:error, "Unsupported protocol"} = CryptoService.encrypt_with_session(invalid_session, "test")
    end

    test "handles decryption errors gracefully" do
      # Try to decrypt with invalid session
      invalid_session = %{session_id: "invalid", protocol: :invalid}

      assert {:error, "Unsupported protocol"} = CryptoService.decrypt_with_session(invalid_session, "test")
    end
  end
end
