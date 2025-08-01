import gleam/int

/// Represents a Signal Protocol session.
pub type Session {
  Session(reference: BitArray)
}

/// Represents a pre-key bundle.
pub type PreKeyBundle {
  PreKeyBundle(
    registration_id: Int,
    identity_key: BitArray,
    pre_key: #(Int, BitArray),
    signed_pre_key: #(Int, BitArray, BitArray),
    base_key: BitArray,
  )
}

/// Represents an identity key pair.
pub type IdentityKeyPair {
  IdentityKeyPair(public_key: BitArray, signature: BitArray)
}

/// Represents a pre-key.
pub type PreKey {
  PreKey(key_id: Int, public_key: BitArray)
}

/// Represents a signed pre-key.
pub type SignedPreKey {
  SignedPreKey(key_id: Int, public_key: BitArray, signature: BitArray)
}

// --- FFI: libsignal_protocol_nif integration ---
@external(erlang, "libsignal_protocol_nif", "generate_identity_key_pair")
fn call_nif_generate_identity_key_pair() -> Result(#(BitArray, BitArray), String)

@external(erlang, "libsignal_protocol_nif", "generate_pre_key")
fn call_nif_generate_pre_key(key_id: Int) -> Result(#(Int, BitArray), String)

@external(erlang, "libsignal_protocol_nif", "generate_signed_pre_key")
fn call_nif_generate_signed_pre_key(
  identity_key: BitArray,
  key_id: Int,
) -> Result(#(Int, BitArray, BitArray), String)

@external(erlang, "libsignal_protocol_nif", "create_session")
fn call_nif_create_session_1(
  public_key: BitArray,
) -> Result(BitArray, String)

@external(erlang, "libsignal_protocol_nif", "create_session")
fn call_nif_create_session_2(
  local_key: BitArray,
  remote_key: BitArray,
) -> Result(BitArray, String)

@external(erlang, "libsignal_protocol_nif", "process_pre_key_bundle")
fn call_nif_process_pre_key_bundle(
  session_ref: BitArray,
  bundle: BitArray,
) -> Result(Nil, String)

@external(erlang, "libsignal_protocol_nif", "encrypt_message")
fn call_nif_encrypt_message(
  session_ref: BitArray,
  message: BitArray,
) -> Result(BitArray, String)

@external(erlang, "libsignal_protocol_nif", "decrypt_message")
fn call_nif_decrypt_message(
  session_ref: BitArray,
  ciphertext: BitArray,
) -> Result(BitArray, String)

// --- Public API ---

/// Generates a new identity key pair.
pub fn generate_identity_key_pair() -> Result(IdentityKeyPair, String) {
  case call_nif_generate_identity_key_pair() {
    Ok(#(public_key, signature)) -> Ok(IdentityKeyPair(public_key, signature))
    Error(reason) -> Error(reason)
  }
}

/// Generates a new pre-key with the given ID.
pub fn generate_pre_key(key_id: Int) -> Result(PreKey, String) {
  case call_nif_generate_pre_key(key_id) {
    Ok(#(key_id, public_key)) -> Ok(PreKey(key_id, public_key))
    Error(reason) -> Error(reason)
  }
}

/// Generates a new signed pre-key with the given ID, signed by the identity key.
pub fn generate_signed_pre_key(
  identity_key: BitArray,
  key_id: Int,
) -> Result(SignedPreKey, String) {
  case call_nif_generate_signed_pre_key(identity_key, key_id) {
    Ok(#(key_id, public_key, signature)) ->
      Ok(SignedPreKey(key_id, public_key, signature))
    Error(reason) -> Error(reason)
  }
}

/// Creates a new session with a public key.
pub fn create_session(
  public_key: BitArray,
) -> Result(Session, String) {
  case call_nif_create_session_1(public_key) {
    Ok(reference) -> Ok(Session(reference))
    Error(reason) -> Error(reason)
  }
}

/// Creates a new session with the given local and remote keys.
pub fn create_session_with_keys(
  local_key: BitArray,
  remote_key: BitArray,
) -> Result(Session, String) {
  case call_nif_create_session_2(local_key, remote_key) {
    Ok(reference) -> Ok(Session(reference))
    Error(reason) -> Error(reason)
  }
}

/// Processes a pre-key bundle to establish a session.
pub fn process_pre_key_bundle(
  session: Session,
  bundle: PreKeyBundle,
) -> Result(Nil, String) {
  let bundle_binary = create_bundle_binary(bundle)
  case
    call_nif_process_pre_key_bundle(session_reference(session), bundle_binary)
  {
    Ok(Nil) -> Ok(Nil)
    Error(reason) -> Error(reason)
  }
}

/// Encrypts a message using the given session.
pub fn encrypt_message(
  session: Session,
  message: BitArray,
) -> Result(BitArray, String) {
  call_nif_encrypt_message(session_reference(session), message)
}

/// Decrypts a message using the given session.
pub fn decrypt_message(
  session: Session,
  ciphertext: BitArray,
) -> Result(BitArray, String) {
  call_nif_decrypt_message(session_reference(session), ciphertext)
}

/// Creates a new session and processes a pre-key bundle in one step.
pub fn create_and_process_bundle(
  local_identity_key: BitArray,
  remote_identity_key: BitArray,
  bundle: PreKeyBundle,
) -> Result(Session, String) {
  let session = create_session_with_keys(local_identity_key, remote_identity_key)
  case session {
    Ok(session) -> {
      case process_pre_key_bundle(session, bundle) {
        Ok(Nil) -> Ok(session)
        Error(e) -> Error(e)
      }
    }
    Error(e) -> Error(e)
  }
}

/// Sends a message through a session, handling encryption.
pub fn send_message(session: Session, message: BitArray) -> Result(BitArray, String) {
  encrypt_message(session, message)
}

/// Receives a message through a session, handling decryption.
pub fn receive_message(
  session: Session,
  ciphertext: BitArray,
) -> Result(BitArray, String) {
  decrypt_message(session, ciphertext)
}

// Helper function to extract the reference from a Session
fn session_reference(session: Session) -> BitArray {
  case session {
    Session(reference) -> reference
  }
}

// Helper function to create a binary representation of a pre-key bundle
fn create_bundle_binary(bundle: PreKeyBundle) -> BitArray {
  // This is a simplified serialization for demonstration
  // In a real implementation, you would use proper binary serialization
  let registration_id_str = int.to_string(bundle.registration_id)
  let #(pre_key_id, _pre_key_public) = bundle.pre_key
  let #(signed_pre_key_id, _signed_pre_key_public, _signed_pre_key_signature) = bundle.signed_pre_key
  
  // For now, just return a simple placeholder binary
  // This is a temporary implementation until proper serialization is needed
  <<registration_id_str:utf8, pre_key_id:32, signed_pre_key_id:32>>
}
