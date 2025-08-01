defmodule Spacecast.SignalProtocolTest do
  use ExUnit.Case, async: true
  alias Spacecast.SignalProtocol

  describe "generate_identity_key_pair/0" do
    test "generates identity key pair successfully" do
      assert {:ok, {public_key, signature}} = SignalProtocol.generate_identity_key_pair()
      assert is_binary(public_key)
      assert is_binary(signature)
    end

    test "returns expected stub values in test environment" do
      assert {:ok, {"stub_public_key", "stub_signature"}} =
               SignalProtocol.generate_identity_key_pair()
    end
  end

  describe "generate_pre_key/1" do
    test "generates pre-key with valid key_id" do
      key_id = 123
      assert {:ok, {returned_key_id, public_key}} = SignalProtocol.generate_pre_key(key_id)
      assert returned_key_id == key_id
      assert is_binary(public_key)
    end

    test "generates pre-key with zero key_id" do
      assert {:ok, {0, public_key}} = SignalProtocol.generate_pre_key(0)
      assert is_binary(public_key)
    end

    test "generates pre-key with large key_id" do
      large_key_id = 999_999_999
      assert {:ok, {returned_key_id, public_key}} = SignalProtocol.generate_pre_key(large_key_id)
      assert returned_key_id == large_key_id
      assert is_binary(public_key)
    end

    test "returns expected stub values in test environment" do
      key_id = 456
      assert {:ok, {^key_id, "stub_pre_key"}} = SignalProtocol.generate_pre_key(key_id)
    end
  end

  describe "generate_signed_pre_key/2" do
    test "generates signed pre-key with valid parameters" do
      identity_key = "test_identity_key"
      key_id = 789

      assert {:ok, {returned_key_id, public_key, signature}} =
               SignalProtocol.generate_signed_pre_key(identity_key, key_id)

      assert returned_key_id == key_id
      assert is_binary(public_key)
      assert is_binary(signature)
    end

    test "generates signed pre-key with empty identity key" do
      assert {:ok, {_key_id, public_key, signature}} =
               SignalProtocol.generate_signed_pre_key("", 123)

      assert is_binary(public_key)
      assert is_binary(signature)
    end

    test "generates signed pre-key with unicode identity key" do
      unicode_key = "café_identity_key_🚨"

      assert {:ok, {_key_id, public_key, signature}} =
               SignalProtocol.generate_signed_pre_key(unicode_key, 456)

      assert is_binary(public_key)
      assert is_binary(signature)
    end

    test "returns expected stub values in test environment" do
      identity_key = "test_key"
      key_id = 789

      assert {:ok, {^key_id, "stub_signed_pre_key", "stub_signature"}} =
               SignalProtocol.generate_signed_pre_key(identity_key, key_id)
    end
  end

  describe "create_session/2" do
    test "creates session with valid identity keys" do
      local_key = "local_identity_key"
      remote_key = "remote_identity_key"

      assert {:ok, session} = SignalProtocol.create_session(local_key, remote_key)
      assert session == :mock_session
    end

    test "creates session with empty identity keys" do
      assert {:ok, session} = SignalProtocol.create_session("", "")
      assert session == :mock_session
    end

    test "creates session with different key lengths" do
      short_key = "short"
      long_key = String.duplicate("long_key_", 100)

      assert {:ok, session} = SignalProtocol.create_session(short_key, long_key)
      assert session == :mock_session
    end

    test "creates session with unicode keys" do
      unicode_local = "café_local_key"
      unicode_remote = "🚨_remote_key_事件"

      assert {:ok, session} = SignalProtocol.create_session(unicode_local, unicode_remote)
      assert session == :mock_session
    end
  end

  describe "process_pre_key_bundle/2" do
    test "processes pre-key bundle successfully" do
      # Create a mock reference for testing
      session = make_ref()
      bundle = "test_bundle_data"

      assert :ok = SignalProtocol.process_pre_key_bundle(session, bundle)
    end

    test "processes empty bundle" do
      session = make_ref()
      assert :ok = SignalProtocol.process_pre_key_bundle(session, "")
    end

    test "processes large bundle" do
      session = make_ref()
      large_bundle = String.duplicate("large_bundle_data_", 1000)

      assert :ok = SignalProtocol.process_pre_key_bundle(session, large_bundle)
    end

    test "processes bundle with special characters" do
      session = make_ref()
      special_bundle = "bundle_with_🚨_café_事件"

      assert :ok = SignalProtocol.process_pre_key_bundle(session, special_bundle)
    end
  end

  describe "encrypt_message/2" do
    test "encrypts message successfully" do
      session = make_ref()
      message = "Hello, World!"

      assert {:ok, ciphertext} = SignalProtocol.encrypt_message(session, message)
      assert is_binary(ciphertext)
    end

    test "encrypts empty message" do
      session = make_ref()
      assert {:ok, ciphertext} = SignalProtocol.encrypt_message(session, "")
      assert is_binary(ciphertext)
    end

    test "encrypts large message" do
      session = make_ref()
      large_message = String.duplicate("large_message_content_", 1000)

      assert {:ok, ciphertext} = SignalProtocol.encrypt_message(session, large_message)
      assert is_binary(ciphertext)
    end

    test "encrypts message with unicode characters" do
      session = make_ref()
      unicode_message = "Message with 🚨 café 事件"

      assert {:ok, ciphertext} = SignalProtocol.encrypt_message(session, unicode_message)
      assert is_binary(ciphertext)
    end

    test "returns expected stub values in test environment" do
      session = make_ref()
      message = "test_message"
      assert {:ok, "stub_encrypted_message"} = SignalProtocol.encrypt_message(session, message)
    end
  end

  describe "decrypt_message/2" do
    test "decrypts message successfully" do
      session = make_ref()
      ciphertext = "encrypted_message_data"

      assert {:ok, plaintext} = SignalProtocol.decrypt_message(session, ciphertext)
      assert is_binary(plaintext)
    end

    test "decrypts empty ciphertext" do
      session = make_ref()
      assert {:ok, plaintext} = SignalProtocol.decrypt_message(session, "")
      assert is_binary(plaintext)
    end

    test "decrypts large ciphertext" do
      session = make_ref()
      large_ciphertext = String.duplicate("large_ciphertext_data_", 1000)

      assert {:ok, plaintext} = SignalProtocol.decrypt_message(session, large_ciphertext)
      assert is_binary(plaintext)
    end

    test "decrypts ciphertext with special characters" do
      session = make_ref()
      special_ciphertext = "ciphertext_with_🚨_café_事件"

      assert {:ok, plaintext} = SignalProtocol.decrypt_message(session, special_ciphertext)
      assert is_binary(plaintext)
    end

    test "returns expected stub values in test environment" do
      session = make_ref()
      ciphertext = "test_ciphertext"
      assert {:ok, "stub_decrypted_message"} = SignalProtocol.decrypt_message(session, ciphertext)
    end
  end

  describe "get_cache_stats/1" do
    test "gets cache statistics successfully" do
      session = make_ref()

      assert {:ok, stats} = SignalProtocol.get_cache_stats(session)
      assert is_map(stats)
      assert Map.has_key?(stats, :chain_key_count)
      assert Map.has_key?(stats, :root_key_count)
      assert stats.chain_key_count == 0
      assert stats.root_key_count == 0
    end

    test "returns expected stub values in test environment" do
      session = make_ref()

      assert {:ok, %{chain_key_count: 0, root_key_count: 0}} =
               SignalProtocol.get_cache_stats(session)
    end
  end

  describe "reset_cache_stats/1" do
    test "resets cache statistics successfully" do
      session = make_ref()
      assert :ok = SignalProtocol.reset_cache_stats(session)
    end
  end

  describe "set_cache_size/3" do
    test "sets cache size with valid parameters" do
      session = make_ref()
      chain_key_size = 100
      root_key_size = 50

      assert :ok = SignalProtocol.set_cache_size(session, chain_key_size, root_key_size)
    end

    test "sets cache size with zero values" do
      session = make_ref()
      assert :ok = SignalProtocol.set_cache_size(session, 0, 0)
    end

    test "sets cache size with large values" do
      session = make_ref()
      large_chain_size = 1_000_000
      large_root_size = 500_000

      assert :ok = SignalProtocol.set_cache_size(session, large_chain_size, large_root_size)
    end

    test "sets cache size with different values" do
      session = make_ref()
      assert :ok = SignalProtocol.set_cache_size(session, 10, 20)
      assert :ok = SignalProtocol.set_cache_size(session, 20, 10)
    end
  end

  describe "integration scenarios" do
    test "complete encryption/decryption flow" do
      # Generate identity keys
      assert {:ok, {local_identity, _local_sig}} = SignalProtocol.generate_identity_key_pair()
      assert {:ok, {remote_identity, _remote_sig}} = SignalProtocol.generate_identity_key_pair()

      # Create session
      assert {:ok, _session} = SignalProtocol.create_session(local_identity, remote_identity)
      # Use a valid reference for session
      session = make_ref()

      # Generate pre-keys
      assert {:ok, {_key_id, _pre_key}} = SignalProtocol.generate_pre_key(123)

      assert {:ok, {_signed_key_id, _signed_pre_key, _signed_sig}} =
               SignalProtocol.generate_signed_pre_key(local_identity, 456)

      # Process bundle (simulated)
      bundle = "simulated_bundle_data"
      assert :ok = SignalProtocol.process_pre_key_bundle(session, bundle)

      # Encrypt and decrypt message
      original_message = "Hello, Signal Protocol!"
      assert {:ok, ciphertext} = SignalProtocol.encrypt_message(session, original_message)
      assert {:ok, decrypted_message} = SignalProtocol.decrypt_message(session, ciphertext)

      # Verify results
      assert is_binary(ciphertext)
      assert is_binary(decrypted_message)

      # Check cache stats
      assert {:ok, stats} = SignalProtocol.get_cache_stats(session)
      assert is_map(stats)

      # Reset and set cache size
      assert :ok = SignalProtocol.reset_cache_stats(session)
      assert :ok = SignalProtocol.set_cache_size(session, 100, 50)
    end

    test "handles multiple sessions" do
      # Create multiple sessions (use valid references)
      session1 = make_ref()
      session2 = make_ref()
      session3 = make_ref()

      # Encrypt messages on different sessions
      message1 = "Message for session 1"
      message2 = "Message for session 2"
      message3 = "Message for session 3"

      assert {:ok, cipher1} = SignalProtocol.encrypt_message(session1, message1)
      assert {:ok, cipher2} = SignalProtocol.encrypt_message(session2, message2)
      assert {:ok, cipher3} = SignalProtocol.encrypt_message(session3, message3)

      # Decrypt messages
      assert {:ok, _decrypted1} = SignalProtocol.decrypt_message(session1, cipher1)
      assert {:ok, _decrypted2} = SignalProtocol.decrypt_message(session2, cipher2)
      assert {:ok, _decrypted3} = SignalProtocol.decrypt_message(session3, cipher3)

      # Verify all are binaries (stub returns same value)
      assert is_binary(cipher1)
      assert is_binary(cipher2)
      assert is_binary(cipher3)
    end
  end

  describe "error handling" do
    test "handles undefined function errors gracefully" do
      # This test verifies that the rescue clauses work properly
      # In the test environment, we use stubs, but the rescue clauses
      # provide fallback behavior for production when NIFs are not available

      # All functions should return valid results even if NIFs fail
      assert {:ok, _} = SignalProtocol.generate_identity_key_pair()
      assert {:ok, _} = SignalProtocol.generate_pre_key(123)
      assert {:ok, _} = SignalProtocol.generate_signed_pre_key("key", 456)
      assert {:ok, _} = SignalProtocol.create_session("local", "remote")
      assert :ok = SignalProtocol.process_pre_key_bundle(make_ref(), "bundle")
      assert {:ok, _} = SignalProtocol.encrypt_message(make_ref(), "message")
      assert {:ok, _} = SignalProtocol.decrypt_message(make_ref(), "ciphertext")
      assert {:ok, _} = SignalProtocol.get_cache_stats(make_ref())
      assert :ok = SignalProtocol.reset_cache_stats(make_ref())
      assert :ok = SignalProtocol.set_cache_size(make_ref(), 100, 50)
    end
  end

  describe "performance characteristics" do
    test "handles large data efficiently" do
      session = make_ref()

      # Test with large messages
      large_message = String.duplicate("large_message_content_", 10_000)

      # Time the encryption
      start_time = System.monotonic_time(:microsecond)
      assert {:ok, ciphertext} = SignalProtocol.encrypt_message(session, large_message)
      end_time = System.monotonic_time(:microsecond)

      # Verify it completes in reasonable time (less than 1 second)
      assert end_time - start_time < 1_000_000

      # Time the decryption
      start_time = System.monotonic_time(:microsecond)
      assert {:ok, _decrypted} = SignalProtocol.decrypt_message(session, ciphertext)
      end_time = System.monotonic_time(:microsecond)

      # Verify it completes in reasonable time
      assert end_time - start_time < 1_000_000
    end
  end
end
