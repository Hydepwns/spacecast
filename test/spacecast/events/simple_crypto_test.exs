defmodule Spacecast.Events.SimpleCryptoTest do
  use ExUnit.Case, async: true
  alias Spacecast.Events.SimpleCrypto

  describe "key generation" do
    test "generates encryption keys" do
      key = SimpleCrypto.generate_key()
      assert is_binary(key)
      assert byte_size(key) == 32
    end

    test "generates initialization vectors" do
      iv = SimpleCrypto.generate_iv()
      assert is_binary(iv)
      assert byte_size(iv) == 16
    end

    test "generates different keys each time" do
      key1 = SimpleCrypto.generate_key()
      key2 = SimpleCrypto.generate_key()
      assert key1 != key2
    end

    test "generates different IVs each time" do
      iv1 = SimpleCrypto.generate_iv()
      iv2 = SimpleCrypto.generate_iv()
      assert iv1 != iv2
    end
  end

  describe "encryption and decryption" do
    test "encrypts and decrypts data" do
      key = SimpleCrypto.generate_key()
      iv = SimpleCrypto.generate_iv()
      plaintext = "Hello, encrypted world!"

      # Encrypt
      assert {:ok, encrypted} = SimpleCrypto.encrypt(plaintext, key, iv)
      assert is_binary(encrypted)
      assert encrypted != plaintext

      # Decrypt
      assert {:ok, decrypted} = SimpleCrypto.decrypt(encrypted, key, iv)
      assert decrypted == plaintext
    end

    test "encrypts with generated key and IV" do
      plaintext = "Test message with generated key"

      assert {:ok, encrypted, key, iv} = SimpleCrypto.encrypt_with_generated_key(plaintext)
      assert is_binary(encrypted)
      assert is_binary(key)
      assert is_binary(iv)
      assert byte_size(key) == 32
      assert byte_size(iv) == 16

      # Decrypt with the generated key and IV
      assert {:ok, decrypted} = SimpleCrypto.decrypt(encrypted, key, iv)
      assert decrypted == plaintext
    end

    test "creates encrypted message format" do
      key = SimpleCrypto.generate_key()
      plaintext = "Message in encrypted format"

      assert {:ok, encrypted_message} = SimpleCrypto.create_encrypted_message(plaintext, key)
      assert is_binary(encrypted_message)
      assert byte_size(encrypted_message) > byte_size(plaintext)

      # Decrypt the message
      assert {:ok, decrypted} = SimpleCrypto.decrypt_message(encrypted_message, key)
      assert decrypted == plaintext
    end

    test "handles empty plaintext" do
      key = SimpleCrypto.generate_key()
      iv = SimpleCrypto.generate_iv()

      assert {:ok, encrypted} = SimpleCrypto.encrypt("", key, iv)
      assert {:ok, decrypted} = SimpleCrypto.decrypt(encrypted, key, iv)
      assert decrypted == ""
    end

    test "handles large plaintext" do
      key = SimpleCrypto.generate_key()
      iv = SimpleCrypto.generate_iv()
      plaintext = String.duplicate("A", 1000)

      assert {:ok, encrypted} = SimpleCrypto.encrypt(plaintext, key, iv)
      assert {:ok, decrypted} = SimpleCrypto.decrypt(encrypted, key, iv)
      assert decrypted == plaintext
    end

    test "fails with wrong key" do
      key1 = SimpleCrypto.generate_key()
      key2 = SimpleCrypto.generate_key()
      iv = SimpleCrypto.generate_iv()
      plaintext = "Test message"

      {:ok, encrypted} = SimpleCrypto.encrypt(plaintext, key1, iv)
      result = SimpleCrypto.decrypt(encrypted, key2, iv)

      # Decryption with wrong key should either fail or produce incorrect plaintext
      case result do
        # Expected padding error
        {:error, _} -> :ok
        # Incorrect plaintext
        {:ok, decrypted} -> assert decrypted != plaintext
      end
    end

    test "fails with wrong IV" do
      key = SimpleCrypto.generate_key()
      iv1 = SimpleCrypto.generate_iv()
      iv2 = SimpleCrypto.generate_iv()
      plaintext = "Test message"

      {:ok, encrypted} = SimpleCrypto.encrypt(plaintext, key, iv1)
      result = SimpleCrypto.decrypt(encrypted, key, iv2)

      # Decryption with wrong IV should either fail or produce incorrect plaintext
      case result do
        # Expected padding error
        {:error, _} -> :ok
        # Incorrect plaintext
        {:ok, decrypted} -> assert decrypted != plaintext
      end
    end

    test "fails with message too short" do
      key = SimpleCrypto.generate_key()
      short_message = "short"

      assert {:error, "Message too short"} = SimpleCrypto.decrypt_message(short_message, key)
    end
  end

  describe "hashing" do
    test "generates SHA-256 hash" do
      data = "Hello, World!"
      hash = SimpleCrypto.sha256(data)

      assert is_binary(hash)
      assert byte_size(hash) == 32

      # Hash should be deterministic
      assert hash == SimpleCrypto.sha256(data)
    end

    test "generates HMAC-SHA256" do
      key = SimpleCrypto.generate_key()
      data = "Message to authenticate"
      hmac = SimpleCrypto.hmac_sha256(key, data)

      assert is_binary(hmac)
      assert byte_size(hmac) == 32

      # HMAC should be deterministic
      assert hmac == SimpleCrypto.hmac_sha256(key, data)
    end

    test "HMAC changes with different keys" do
      key1 = SimpleCrypto.generate_key()
      key2 = SimpleCrypto.generate_key()
      data = "Same data, different keys"

      hmac1 = SimpleCrypto.hmac_sha256(key1, data)
      hmac2 = SimpleCrypto.hmac_sha256(key2, data)

      assert hmac1 != hmac2
    end

    test "HMAC changes with different data" do
      key = SimpleCrypto.generate_key()
      data1 = "First message"
      data2 = "Second message"

      hmac1 = SimpleCrypto.hmac_sha256(key, data1)
      hmac2 = SimpleCrypto.hmac_sha256(key, data2)

      assert hmac1 != hmac2
    end
  end

  describe "random string generation" do
    test "generates random strings" do
      length = 16
      random_string = SimpleCrypto.random_string(length)

      assert is_binary(random_string)
      assert String.length(random_string) == length
    end

    test "generates different strings each time" do
      length = 20
      string1 = SimpleCrypto.random_string(length)
      string2 = SimpleCrypto.random_string(length)

      assert string1 != string2
    end

    test "handles zero length" do
      assert "" == SimpleCrypto.random_string(0)
    end
  end

  describe "padding" do
    test "handles padding correctly" do
      # Test with data that needs padding
      key = SimpleCrypto.generate_key()
      iv = SimpleCrypto.generate_iv()

      # Test with different lengths
      lengths = [1, 15, 16, 17, 31, 32, 33]

      for length <- lengths do
        plaintext = String.duplicate("A", length)

        assert {:ok, encrypted} = SimpleCrypto.encrypt(plaintext, key, iv)
        assert {:ok, decrypted} = SimpleCrypto.decrypt(encrypted, key, iv)
        assert decrypted == plaintext
      end
    end
  end

  describe "error handling" do
    test "handles invalid parameters" do
      # Test with nil values
      assert {:error, _} = SimpleCrypto.encrypt(nil, "key", "iv")
      assert {:error, _} = SimpleCrypto.encrypt("data", nil, "iv")
      assert {:error, _} = SimpleCrypto.encrypt("data", "key", nil)

      assert {:error, _} = SimpleCrypto.decrypt(nil, "key", "iv")
      assert {:error, _} = SimpleCrypto.decrypt("data", nil, "iv")
      assert {:error, _} = SimpleCrypto.decrypt("data", "key", nil)
    end
  end
end
