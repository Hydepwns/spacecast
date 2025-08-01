# Crypto Service Documentation

## Overview

The `Spacecast.Events.CryptoService` module provides secure message encryption and decryption using either Signal Protocol (when available) or a fallback AES-256-CBC implementation. This service is designed to handle secure communication between parties with automatic session management.

## Features

- **Dual Protocol Support**: Uses Signal Protocol when available, falls back to AES-256-CBC
- **Session Management**: Automatic creation and management of encryption sessions
- **Digital Signatures**: Ed25519 signatures with HMAC-SHA256 fallback
- **Persistent Storage**: ETS-based session storage with automatic cleanup
- **Error Handling**: Comprehensive error handling and logging
- **Statistics**: Session and message statistics tracking

## Architecture

### Protocol Selection

The service automatically detects if Signal Protocol is available and chooses the appropriate encryption method:

1. **Signal Protocol** (Primary): Uses the existing `libsignal-protocol-nif` implementation
2. **Simple Crypto** (Fallback): Uses Erlang's crypto module with AES-256-CBC

### Session Architecture

Sessions are stored in an ETS table (`:signal_sessions`) and include:

- Session ID
- Local and remote user IDs
- Encryption keys (protocol-specific)
- Creation timestamp
- Message count
- Protocol type (`:signal` or `:simple`)

## Usage

### Initialization

```elixir
# Initialize the crypto service (call during application startup)
Spacecast.Events.CryptoService.init()
```

### Basic Encryption/Decryption

```elixir
# Encrypt a message
reminder = %{message: "Hello, secure world!"}
settings = %{recipient_id: "user123"}

case Spacecast.Events.CryptoService.encrypt_message(reminder, settings) do
  {:ok, encrypted_message} ->
    # Send encrypted_message to recipient
    send_secure_message(recipient, encrypted_message)
  
  {:error, reason} ->
    Logger.error("Encryption failed: #{reason}")
end

# Decrypt a message
case Spacecast.Events.CryptoService.decrypt_message(encrypted_message, session_id) do
  {:ok, decrypted_message} ->
    # Process decrypted_message
    process_message(decrypted_message)
  
  {:error, reason} ->
    Logger.error("Decryption failed: #{reason}")
end
```

### Session Operations

```elixir
# Create a new session
{:ok, session_id} = Spacecast.Events.CryptoService.create_session("user1", "user2")

# Get session information
{:ok, session} = Spacecast.Events.CryptoService.get_session(session_id)

# List all sessions
sessions = Spacecast.Events.CryptoService.list_sessions()

# Delete a session
:ok = Spacecast.Events.CryptoService.delete_session(session_id)
```

### Digital Signatures

```elixir
# Sign data
data = "Important message"
private_key = :crypto.strong_rand_bytes(32)

{:ok, signature} = Spacecast.Events.CryptoService.sign_data(private_key, data)

# Verify signature
{:ok, is_valid} = Spacecast.Events.CryptoService.verify_signature(private_key, data, signature)
```

### Direct Session Operations

```elixir
# Create session and get session object
{:ok, session_id} = Spacecast.Events.CryptoService.create_session("user1", "user2")
{:ok, session} = Spacecast.Events.CryptoService.get_session(session_id)

# Encrypt with session directly
{:ok, encrypted} = Spacecast.Events.CryptoService.encrypt_with_session(session, "Hello")

# Decrypt with session directly
{:ok, decrypted} = Spacecast.Events.CryptoService.decrypt_with_session(session, encrypted)
```

### Statistics and Maintenance

```elixir
# Get session statistics
stats = Spacecast.Events.CryptoService.get_session_stats()
# Returns: %{
#   total_sessions: 5,
#   total_messages: 42,
#   oldest_session: %DateTime{},
#   newest_session: %DateTime{}
# }

# Clean up expired sessions (older than 24 hours)
cleaned_count = Spacecast.Events.CryptoService.cleanup_expired_sessions()
```

## API Reference

### Core Functions

#### `init/0`

Initializes the crypto service and session storage.

#### `encrypt_message/2`

Encrypts a message for a recipient.

- `reminder`: Map containing the message to encrypt
- `settings`: Map containing recipient information

#### `decrypt_message/2`

Decrypts a message using a session ID.

- `encrypted_message`: Base64-encoded encrypted message
- `session_id`: Session identifier

#### `create_session/2`

Creates a new encryption session between two parties.

- `local_user_id`: ID of the local user
- `remote_user_id`: ID of the remote user

### Session Management Functions

#### `get_session/1`

Retrieves session information by ID.

#### `list_sessions/0`

Returns all active sessions.

#### `delete_session/1`

Deletes a session by ID.

#### `get_session_stats/0`

Returns statistics about all sessions.

#### `cleanup_expired_sessions/0`

Removes sessions older than 24 hours.

### Cryptographic Operations

#### `sign_data/2`

Signs data using digital signatures.

#### `verify_signature/3`

Verifies digital signatures.

#### `encrypt_with_session/2`

Encrypts a message using a session object.

#### `decrypt_with_session/2`

Decrypts a message using a session object.

## Security Considerations

### Signal Protocol

- Uses libsodium for cryptographic operations
- Implements Double Ratchet algorithm
- Provides forward secrecy and post-compromise security
- Uses Curve25519 for key exchange and Ed25519 for signatures

### Simple Crypto Fallback

- Uses AES-256-CBC with PKCS7 padding
- Generates random IVs for each encryption
- Uses HMAC-SHA256 for message authentication
- Provides basic confidentiality and integrity

### Session Security

- Sessions are stored in memory (ETS)
- Automatic cleanup of expired sessions
- Unique session IDs for each conversation
- Message counting for session tracking

## Error Handling

The service provides comprehensive error handling:

```elixir
# Common error patterns
{:error, "Encryption failed"}           # General encryption error
{:error, "Decryption failed"}           # General decryption error
{:error, "Session creation failed"}     # Session creation error
{:error, "Invalid encrypted message format"} # Malformed message
{:error, "Unsupported protocol"}        # Unknown protocol type
{:error, :not_found}                    # Session not found
```

## Testing

The service includes comprehensive tests:

```bash
# Run crypto service tests
mix test test/spacecast/events/crypto_service_test.exs

# Run simple crypto tests
mix test test/spacecast/events/simple_crypto_test.exs
```

## Dependencies

### Required

- `:crypto` - Erlang crypto module (built-in)
- `:ets` - Erlang Term Storage (built-in)

### Optional

- `signal_nif` - Signal Protocol NIF (from `lib/libsignal-protocol-nif`)

## Configuration

The service is configured in `mix.exs`:

```elixir
# Add Signal Protocol for secure messaging
{:signal_nif, path: "lib/libsignal-protocol-nif", manager: :rebar3},
```

## Performance Considerations

- Sessions are stored in ETS for fast access
- Automatic cleanup prevents memory leaks
- Message counting is updated atomically
- Error handling includes proper logging

## Future Enhancements

- Database persistence for sessions
- Key rotation mechanisms
- Multi-party encryption support
- Integration with external key management systems
- Performance metrics and monitoring
