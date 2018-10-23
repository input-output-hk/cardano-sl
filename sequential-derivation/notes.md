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
 * SLIP-0044
  

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
Derive private key from a parent private key. Root key is given with `XPrv 0`. I am not sure how exactly `Index` is defined (UPDATE: later in text this might be referred as address index - an ever increasing number). Its probably just a Natural number (in this case 1).

UPDATE: later in text it is cleared out that private key derivation from root key will be used to derive other private keys which will have use cases such as: change, address, account, ... So every user has single root private key, and from this key we are deriving multiple private keys for each needed use case. In practice this will probably be a small number of use cases and thus a small number of private keys derived from root private key. This also tells us that `toPub . derivePrivate :: XPrv n -> Index -> XPub (n+1)` parent private key can derive any public key.

```
derivePublic :: XPub n -> (Index < 0x80000000) -> XPub (n+1)
```
Derive public key from a parent public key. Root key is given with `XPub 0`. I am not sure what `Index < 0x80000000` means. It might be constrain that public key Index is upper bounded. This might be the answer to how many public keys can be generated from a root public key? (it might also be some sort of shifting operation though). 

> Given an issue in the original ED25519-BIP32 implementation
TODO: check which issue are we referring to here

Document states that we already have two schemes implemented in V0 (deprecated) and V1. How do they differ? (seems like alfredo's gist it explaining how current random scheme differs from this one. What means scheme is random? https://input-output-rnd.slack.com/archives/GBT05825V/p1540276677000100?thread_ts=1540276105.000100&cid=GBT05825V)

## Derivation path levels

Hard vs soft derivation from bip44: how do they differ?
We are using hard derivation for: coin_type (what is this?), account, purpose (what is this?)
We are using soft derivation for: change (change address or?), address index, m (what is this?)

`purpose` and `coin_type` are hardcoded values for hard derivation (probably some sort of hardcoded configuration/parameters)

TYPO:
> multiple possible account for a given
multiple possible accounts for a given

`account` is a hard derivation
`change` is a constant to differentiate internal and external addresses. What is a use case for internal and/or external addresses? Are internal addresses addresses possessed by us (derived by our root key) where external addresses might be addresses derived from different master root key. Is there an upper bound to address index (my intuition is that private key entropy is not unlimited resource - from which we derive other keys). Or are internal addresses addresses created by internal wallet (ie, normal wallet) and external addresses are addresses created by external wallet (like hardware wallet)?

TYPO:
> the root keymis of typeXPrv 0'
the root keys are of type XPrv 0'

TYPO:
> the coin type level keys are of type XPrv 2
the coin_type ...

root, purpose, coin_type, account, change, address :: XPrv [0..5]

`purpose` value is hardcoded to 44'
`coin_type` value is hardcoded to 1851' (0x80000717; 1815 is Ada Lovelace birthdate)
TODO: check SLIP-0044 for more details

This is a bit confusing to me. So my understanding now is that we have `purpose` and `coin_type` values, but we also have `purpose` level keys (of type `XPrv 1`) and `coin_type` (of type `XPrv 2`), right?

## Motivation

> New Account derivation requires the level 2 private key
We previously said that `account` level keys are of type `XPrv 3`. Shouldn't this be level 3 private key, if keys are indexed from root zero level private key?

> Caching account public key `XPub 3` allows derivation of `XPrv 5` without the private key
That means we are able to derive addresses from account public keys. This is not obvious from functions given at Derivation function section:

```
toPub :: XPrv n -> XPub n
derivePrivate :: XPrv n -> Index -> XPrv (n+1)
derivePublic :: XPub n -> (Index < 0x80000000) -> XPub (n+1)
```

I can't find a way how to derive any private key from a public one given these functions. I must be missing some function!
This probably also means that we can derive `XPrv 5` private key from `XPub 4` public key? If so, that means that change level keys can also derive addresses? (are change level keys intended for external wallets use case?)

> Accounts provide extra insulation against catastrophic leaking of cryptographic material
> in the account itself, see caveat of soft derivation in particular. More specifically if the
> tree of derivation of a given account is compromised, then the hard derivation allows, in
> theory, other accounts to remain secure.
Is hard derivation actually function doing `XPrv n -> ... -> XPrv (n+1)` and/or `XPrv n -> XPub n`? And in a similar way soft derivation is actually function doing `XPub n -> ... -> XPub (n+1)` and/or `XPub n -> ... -> XPrv (n+2)`? Is difference between soft derivation and hard derivation such that soft derivation is a derivation that derives keys (either private or public) only from public key. That way, if public key is compromised, user can still remain secure (in theory)? And hard derivation is derivation which derives keys using private keys?
TODO: google "soft derivation wiki". So I guess accounts have been created with hard derivation (from any parent private key) where addresses have been created using soft derivation (from an account public key: `XPub account -> XPrv address` - this should be soft derivation). Then if `XPub account` is compromised, other accounts that have been derived are in theory still safe as attacker can't derive them using `XPub account` (I wonder how this is different from having `XPrv account`; probably depends what keys are signing transactions).
