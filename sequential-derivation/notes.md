# Notes 

My strategy will be to read documents in this order:
 * Sequential derivation scheme pdf - https://drive.google.com/file/d/15hjAoZ-IPeex71SPVsuHKk6MCOhE5XsQ/view
 * bip32 proposal - https://github.com/bitcoin/bips/blob/master/bip-0032.mediawiki
 * bip44 proposal - https://github.com/bitcoin/bips/blob/master/bip-0044.mediawiki
 * bip39 proposal - https://github.com/bitcoin/bips/blob/master/bip-0039.mediawiki
    - mnemonic scheme is already familiar to me
 * ED25519 and ED25519-BIP32 schemes
 * alfredo's gist - https://gist.github.com/adinapoli-iohk/1059c383c3c956a99150be297c7e0c75
 * reread the Mastering Bitcoin sections on BIP 32/44 (in Chapter 5 on wallets) - advice from Ryan
  

After reading "Sequential derivation scheme" for the first time I would expect to have a vague understanding of some primitives. Will do a second read after reading bip32, bip44 and other proposals as they will probably shed a light on Sequential derivation scheme.

# Reading "Sequential derivation scheme" pdf

If this is pdf produced by us, there are some typos. I can create a PR if I knew the repo:

## Abstract

TYPO:
> Describe the sequential scheme wallet derivation scheme
probably should be "Describe the sequential wallet derivation scheme"

What means that scheme is sequential?
What is exactly wallet derivation? Is that private/public key derivation from some master key? Usually master key which enables derivation is a private master key - but to my understanding hardware/external wallets require different use case: to have a master public key which can derive other private/public key pairs and to use master private key only to sign created transactions (this key should be held on a external/hardware device).

Document says it additionally covers recovery (I guess we mean some catastrophic recovery option - like losing a key. In such case user has to have an option to recover a key) and details for multisig accounts. My understanding for multisig accounts is that its a scheme where multiple signatures are needed for transaction to be accepted by a ledger.

## Derivation function

For derivation we are using ED25519-BIP32 scheme, which is bip32 with ED25519 crypto signing scheme.

TYPO:
> (ED21559), which is defined in ED25519-BIP32.
s/ED21559/ED25519

We are given 3 functions for deriving keys:

```
toPub :: XPrv n -> XPub n
```
Derive public key from a private one. Its more extraction in this case as private key probably contains public key within.

`n` is a Natural number representing depth of the derivation. I would guess this is a deterministic way of generating more keys. How many entropy do we have and how many keys will be able to safely derive before depleting the entropy? (this is probably defined in either ED25519 or bip32)

```
derivePrivate :: XPrv n -> Index -> XPrv (n+1)
```
Derive private key from a parent private key. Root key is given with `XPrv 0`. I am not sure how exactly `Index` is defined. Its probably just a Natural number (in this case 1).

```
derivePublic :: XPub n -> (Index < 0x80000000) -> XPub (n+1)
```
Derive public key from a parent public key. Root key is given with `XPub 0`. I am not sure what `Index < 0x80000000` means. It might be constrain that public key Index is upper bounded. This might be the answer to how many public keys can be generated from a root public key? (it might also be some sort of shifting operation though)

> Given an issue in the original ED25519-BIP32 implementation
TODO: check which issue are we referring to here

Document states that we already have two schemes implemented in V0 (deprecated) and V1. How do they differ? (seems like alfredo's gist it explaining how curren random scheme differs from this one. What means scheme is random? https://input-output-rnd.slack.com/archives/GBT05825V/p1540276677000100?thread_ts=1540276105.000100&cid=GBT05825V)

## Derivation path levels

Hard vs soft derivation from bip44: how do they differ?
We are using hard derivation for: coin_type (what is this?), account, purpose (what is this?)
We are using soft derivation for: change (change address or?), address index, m (what is this?)

purpose and coin_type are hardcoded values for hard derivation (probably some sort of hardcoded configuration/parameters)
