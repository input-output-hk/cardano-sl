# cardano-sl-crypto

The cryptographic primitives used in Cardano SL.

* A simplified interface to access the AES encryption functionality in the
  [cryptonite] library. It uses AES256 CTR mode with IV = 0.
* Cryptographic hashing (again using the [cryptonite] library).
* A wrapper around the [scrypt] library used for password-based key derivation
  functions.
* A wrapper around [pvss-haskell] for Verifiable Secret Sharing (VSS).
* Secure generation of cryptographically random numbers and `ByteString`s.
* Hierarchical derivation functionality for Hierarchical Deterministic key
  creation (ie for the wallet).
* Cryptographic signing and signature checking.
* `Aeson` and `Bi` (serialisation, see the cardano-sl-binary package) instances
  for the cryptographic data types.

[cryptonite]: https://hackage.haskell.org/package/cryptonite
[pvss-haskell]: https://github.com/input-output-hk/pvss-haskell
[scrypt]: https://hackage.haskell.org/package/scrypt
