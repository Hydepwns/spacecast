#include <erl_nif.h>
#include <string.h>
#include <stdlib.h>
#include <sodium.h>

static ERL_NIF_TERM test_function(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM test_crypto(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    return enif_make_atom(env, "crypto_ok");
}

static ERL_NIF_TERM sha256(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    if (argc != 1) {
        return enif_make_badarg(env);
    }
    
    ErlNifBinary data;
    if (!enif_inspect_binary(env, argv[0], &data)) {
        return enif_make_badarg(env);
    }
    
    // Generate real SHA-256 hash using libsodium
    unsigned char hash[crypto_hash_sha256_BYTES];
    crypto_hash_sha256(hash, data.data, data.size);
    
    ERL_NIF_TERM hash_term;
    unsigned char *bin_data = enif_make_new_binary(env, 32, &hash_term);
    memcpy(bin_data, hash, 32);
    return enif_make_tuple2(env, enif_make_atom(env, "ok"), hash_term);
}

static ERL_NIF_TERM generate_curve25519_keypair(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    if (argc != 0) {
        return enif_make_badarg(env);
    }
    
    // Generate real Curve25519 key pair using libsodium
    unsigned char public_key[32];
    unsigned char private_key[32];
    
    // Use libsodium's crypto_box_keypair which generates Curve25519 keys
    if (crypto_box_keypair(public_key, private_key) != 0) {
        return enif_make_tuple2(env, enif_make_atom(env, "error"), 
                               enif_make_atom(env, "key_generation_failed"));
    }
    
    ERL_NIF_TERM private_term, public_term;
    unsigned char *private_data = enif_make_new_binary(env, 32, &private_term);
    unsigned char *public_data = enif_make_new_binary(env, 32, &public_term);
    
    memcpy(private_data, private_key, 32);
    memcpy(public_data, public_key, 32);
    
    // Clear sensitive data from stack
    sodium_memzero(private_key, sizeof(private_key));
    
    return enif_make_tuple2(env, enif_make_atom(env, "ok"), 
                           enif_make_tuple2(env, private_term, public_term));
}

static ERL_NIF_TERM generate_ed25519_keypair(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    if (argc != 0) {
        return enif_make_badarg(env);
    }
    
    // Generate Ed25519 key pair using libsodium
    unsigned char public_key[crypto_sign_PUBLICKEYBYTES];
    unsigned char private_key[crypto_sign_SECRETKEYBYTES];
    
    if (crypto_sign_keypair(public_key, private_key) != 0) {
        return enif_make_tuple2(env, enif_make_atom(env, "error"), 
                               enif_make_atom(env, "key_generation_failed"));
    }
    
    ERL_NIF_TERM private_term, public_term;
    // Ed25519 private key seed is the first 32 bytes of the 64-byte private key
    unsigned char *private_data = enif_make_new_binary(env, 32, &private_term);
    unsigned char *public_data = enif_make_new_binary(env, crypto_sign_PUBLICKEYBYTES, &public_term);
    
    // Extract the seed (first 32 bytes) from the private key
    memcpy(private_data, private_key, 32);
    memcpy(public_data, public_key, crypto_sign_PUBLICKEYBYTES);
    
    // Clear sensitive data from stack
    sodium_memzero(private_key, sizeof(private_key));
    
    return enif_make_tuple2(env, enif_make_atom(env, "ok"), 
                           enif_make_tuple2(env, public_term, private_term));
}

static ERL_NIF_TERM sign_data(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    if (argc != 2) {
        return enif_make_badarg(env);
    }
    
    ErlNifBinary private_key, message;
    if (!enif_inspect_binary(env, argv[0], &private_key) ||
        !enif_inspect_binary(env, argv[1], &message)) {
        return enif_make_badarg(env);
    }
    
    // Validate private key size (expecting 32-byte seed)
    if (private_key.size != 32) {
        return enif_make_tuple2(env, enif_make_atom(env, "error"), 
                               enif_make_atom(env, "invalid_private_key"));
    }
    
    // Convert 32-byte seed to 64-byte private key
    unsigned char full_private_key[crypto_sign_SECRETKEYBYTES];
    unsigned char public_key_temp[crypto_sign_PUBLICKEYBYTES];
    crypto_sign_seed_keypair(public_key_temp, full_private_key, private_key.data);
    
    // Sign the message
    unsigned char signature[crypto_sign_BYTES];
    unsigned long long signature_len;
    
    if (crypto_sign_detached(signature, &signature_len, message.data, message.size, full_private_key) != 0) {
        return enif_make_tuple2(env, enif_make_atom(env, "error"), 
                               enif_make_atom(env, "signing_failed"));
    }
    
    ERL_NIF_TERM signature_term;
    unsigned char *signature_data = enif_make_new_binary(env, signature_len, &signature_term);
    memcpy(signature_data, signature, signature_len);
    
    // Clear sensitive data
    sodium_memzero(full_private_key, sizeof(full_private_key));
    
    return enif_make_tuple2(env, enif_make_atom(env, "ok"), signature_term);
}

static ERL_NIF_TERM verify_signature(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    if (argc != 3) {
        return enif_make_badarg(env);
    }
    
    ErlNifBinary public_key, message, signature;
    if (!enif_inspect_binary(env, argv[0], &public_key) ||
        !enif_inspect_binary(env, argv[1], &message) ||
        !enif_inspect_binary(env, argv[2], &signature)) {
        return enif_make_badarg(env);
    }
    
    // Validate public key size
    if (public_key.size != crypto_sign_PUBLICKEYBYTES) {
        return enif_make_tuple2(env, enif_make_atom(env, "error"), 
                               enif_make_atom(env, "invalid_public_key"));
    }
    
    // Validate signature size
    if (signature.size != crypto_sign_BYTES) {
        return enif_make_tuple2(env, enif_make_atom(env, "error"), 
                               enif_make_atom(env, "invalid_signature"));
    }
    
    // Verify the signature
    if (crypto_sign_verify_detached(signature.data, message.data, message.size, public_key.data) == 0) {
        return enif_make_atom(env, "ok");
    } else {
        return enif_make_atom(env, "invalid_signature");
    }
}

static ERL_NIF_TERM sha512(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    if (argc != 1) {
        return enif_make_badarg(env);
    }
    
    ErlNifBinary data;
    if (!enif_inspect_binary(env, argv[0], &data)) {
        return enif_make_badarg(env);
    }
    
    // Generate SHA-512 hash using libsodium
    unsigned char hash[crypto_hash_sha512_BYTES];
    crypto_hash_sha512(hash, data.data, data.size);
    
    ERL_NIF_TERM hash_term;
    unsigned char *bin_data = enif_make_new_binary(env, crypto_hash_sha512_BYTES, &hash_term);
    memcpy(bin_data, hash, crypto_hash_sha512_BYTES);
    return enif_make_tuple2(env, enif_make_atom(env, "ok"), hash_term);
}

static ERL_NIF_TERM hmac_sha256(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    if (argc != 2) {
        return enif_make_badarg(env);
    }
    
    ErlNifBinary key, data;
    if (!enif_inspect_binary(env, argv[0], &key) ||
        !enif_inspect_binary(env, argv[1], &data)) {
        return enif_make_badarg(env);
    }
    
    // Generate HMAC-SHA256 using libsodium
    unsigned char hmac[crypto_auth_BYTES];
    if (crypto_auth(hmac, data.data, data.size, key.data) != 0) {
        return enif_make_tuple2(env, enif_make_atom(env, "error"), 
                               enif_make_atom(env, "hmac_failed"));
    }
    
    ERL_NIF_TERM hmac_term;
    unsigned char *hmac_data = enif_make_new_binary(env, crypto_auth_BYTES, &hmac_term);
    memcpy(hmac_data, hmac, crypto_auth_BYTES);
    return enif_make_tuple2(env, enif_make_atom(env, "ok"), hmac_term);
}

static ERL_NIF_TERM aes_gcm_encrypt(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    if (argc != 5) {
        return enif_make_badarg(env);
    }
    
    ErlNifBinary key, iv, plaintext, aad;
    int tag_len;
    
    if (!enif_inspect_binary(env, argv[0], &key) ||
        !enif_inspect_binary(env, argv[1], &iv) ||
        !enif_inspect_binary(env, argv[2], &plaintext) ||
        !enif_inspect_binary(env, argv[3], &aad) ||
        !enif_get_int(env, argv[4], &tag_len)) {
        return enif_make_badarg(env);
    }
    
    // Validate key and IV sizes
    if (key.size != crypto_aead_aes256gcm_KEYBYTES || 
        iv.size != crypto_aead_aes256gcm_NPUBBYTES ||
        tag_len != crypto_aead_aes256gcm_ABYTES) {
        return enif_make_tuple2(env, enif_make_atom(env, "error"), 
                               enif_make_atom(env, "invalid_parameters"));
    }
    
    // Check if AES-GCM is available
    if (!crypto_aead_aes256gcm_is_available()) {
        return enif_make_tuple2(env, enif_make_atom(env, "error"), 
                               enif_make_atom(env, "aes_gcm_not_available"));
    }
    
    // Allocate memory for ciphertext + tag
    size_t ciphertext_len = plaintext.size + crypto_aead_aes256gcm_ABYTES;
    ERL_NIF_TERM ciphertext_term, tag_term;
    unsigned char *ciphertext_data = enif_make_new_binary(env, plaintext.size, &ciphertext_term);
    unsigned char *tag_data = enif_make_new_binary(env, crypto_aead_aes256gcm_ABYTES, &tag_term);
    
    // Temporary buffer for ciphertext + tag
    unsigned char *temp_buffer = malloc(ciphertext_len);
    if (!temp_buffer) {
        return enif_make_tuple2(env, enif_make_atom(env, "error"), 
                               enif_make_atom(env, "memory_allocation_failed"));
    }
    
    unsigned long long actual_ciphertext_len;
    if (crypto_aead_aes256gcm_encrypt(temp_buffer, &actual_ciphertext_len,
                                      plaintext.data, plaintext.size,
                                      aad.data, aad.size,
                                      NULL, iv.data, key.data) != 0) {
        free(temp_buffer);
        return enif_make_tuple2(env, enif_make_atom(env, "error"), 
                               enif_make_atom(env, "encryption_failed"));
    }
    
    // Split ciphertext and tag
    memcpy(ciphertext_data, temp_buffer, plaintext.size);
    memcpy(tag_data, temp_buffer + plaintext.size, crypto_aead_aes256gcm_ABYTES);
    
    free(temp_buffer);
    return enif_make_tuple3(env, enif_make_atom(env, "ok"), ciphertext_term, tag_term);
}

static ERL_NIF_TERM aes_gcm_decrypt(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    if (argc != 6) {
        return enif_make_badarg(env);
    }
    
    ErlNifBinary key, iv, ciphertext, aad, tag;
    int expected_plaintext_len;
    
    if (!enif_inspect_binary(env, argv[0], &key) ||
        !enif_inspect_binary(env, argv[1], &iv) ||
        !enif_inspect_binary(env, argv[2], &ciphertext) ||
        !enif_inspect_binary(env, argv[3], &aad) ||
        !enif_inspect_binary(env, argv[4], &tag) ||
        !enif_get_int(env, argv[5], &expected_plaintext_len)) {
        return enif_make_badarg(env);
    }
    
    // Validate parameters
    if (key.size != crypto_aead_aes256gcm_KEYBYTES || 
        iv.size != crypto_aead_aes256gcm_NPUBBYTES ||
        tag.size != crypto_aead_aes256gcm_ABYTES ||
        expected_plaintext_len != (int)ciphertext.size) {
        return enif_make_tuple2(env, enif_make_atom(env, "error"), 
                               enif_make_atom(env, "invalid_parameters"));
    }
    
    // Check if AES-GCM is available
    if (!crypto_aead_aes256gcm_is_available()) {
        return enif_make_tuple2(env, enif_make_atom(env, "error"), 
                               enif_make_atom(env, "aes_gcm_not_available"));
    }
    
    // Create combined ciphertext + tag buffer
    size_t combined_len = ciphertext.size + tag.size;
    unsigned char *combined_buffer = malloc(combined_len);
    if (!combined_buffer) {
        return enif_make_tuple2(env, enif_make_atom(env, "error"), 
                               enif_make_atom(env, "memory_allocation_failed"));
    }
    
    memcpy(combined_buffer, ciphertext.data, ciphertext.size);
    memcpy(combined_buffer + ciphertext.size, tag.data, tag.size);
    
    ERL_NIF_TERM plaintext_term;
    unsigned char *plaintext_data = enif_make_new_binary(env, expected_plaintext_len, &plaintext_term);
    
    unsigned long long actual_plaintext_len;
    if (crypto_aead_aes256gcm_decrypt(plaintext_data, &actual_plaintext_len,
                                      NULL,
                                      combined_buffer, combined_len,
                                      aad.data, aad.size,
                                      iv.data, key.data) != 0) {
        free(combined_buffer);
        return enif_make_tuple2(env, enif_make_atom(env, "error"), 
                               enif_make_atom(env, "decryption_failed"));
    }
    
    free(combined_buffer);
    return enif_make_tuple2(env, enif_make_atom(env, "ok"), plaintext_term);
}

// Define the NIF function array with the correct 4-field structure for Erlang 27
static ErlNifFunc nif_funcs[] = {
    {"test_function", 0, test_function, 0},
    {"test_crypto", 0, test_crypto, 0},
    {"sha256", 1, sha256, 0},
    {"generate_curve25519_keypair", 0, generate_curve25519_keypair, 0},
    {"generate_ed25519_keypair", 0, generate_ed25519_keypair, 0},
    {"sign_data", 2, sign_data, 0},
    {"verify_signature", 3, verify_signature, 0},
    {"sha512", 1, sha512, 0},
    {"hmac_sha256", 2, hmac_sha256, 0},
    {"aes_gcm_encrypt", 5, aes_gcm_encrypt, 0},
    {"aes_gcm_decrypt", 6, aes_gcm_decrypt, 0}
};

static int on_load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info)
{
    // Initialize libsodium
    if (sodium_init() < 0) {
        return -1; // Failed to initialize libsodium
    }
    return 0;
}

static void on_unload(ErlNifEnv *env, void *priv_data)
{
}

// Initialize the NIF library
ERL_NIF_INIT(signal_nif, nif_funcs, on_load, NULL, NULL, on_unload)

 