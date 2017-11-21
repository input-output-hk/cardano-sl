# Threshold signatures for CSL

This document presents technical requirement draft for introducing threshold signatures into CSL.

Note: document is aimed to cover work to be performed within Shelley, we should concentrate on solutions affordable within this time and content scope.

Document is structured as follows:

1. Introduction: agree on terminology
2. Requirements: what functionality is required, desirable
3. Existing implementation
4. Possible solutions

## Introduction

Threshold cryptosystem is defined as [here](https://en.wikipedia.org/wiki/Threshold_cryptosystem)

We use some definitions in natural way:

 - threshold public key (PK)
 - threshold signature

### HD wallets

We also will talk about HD wallets.

 - BTC implementation is described in [BIP-32](https://github.com/bitcoin/bips/blob/master/bip-0032.mediawiki)
 - CSL is provided with almost equivalent implementation
 - More technical details on CSL implementation of HD wallets are described in [document](https://github.com/input-output-hk/cardano-sl/blob/master/docs/hd.md)
 - CSL uses Ed25519, so approach from [the paper](https://www.ietf.org/proceedings/interim-2017-cfrg-01/slides/slides-interim-2017-cfrg-01-sessa-bip32-ed25519-01.pdf) was applied

For HD wallets we use terms:
	
 - *Derivation* -- operation of seriving child secret key from parent
    * In our case, derivation is operation taking secret/public key and index, returning new secret/public key
 - *HD root key* -- root of HD wallet (single top-most key in derivation tree)
 - *Child* and *parent* keys -- relation in derivation hierarchy
 - *Level* -- depth of derived key in derivation tree (level 0 is assigned to root)

Additionally, BIP-32 specifies two types of derivation (same facility is implemented in CSL):

 - Hardened: `parent SK -> child SK` (child SK can be derived from parent SK)
 - Non-hardened: `parent PK -> child PK`, as well as `parent SK -> child SK`

## Requirements

For CSL we need basic threshold signature support:

 - (R1) Threshold signature scheme: given threshold PK and signature (constructed with M of N parties)
 - (R2) Protocol to construct threshold PK
 - (R3) Protocol to construct threshold signature

Additionally, we need some support of HD wallets for these threshold signatures. In particular:

 - (H1) N parties should be able to create M-of-N wallet `W` 
 - (H2) Each of N shared wallet owners is able to create child account `Acc` for wallet `W`, child address `Addr` for account `Acc`
 - (H3) M of N wallet owners should be able to collaboratively sign transaction from address belonging to `W` hierarchy in order to spend funds

These H1-H3 result in following definitions with regard to threshold scheme terminology:
	
 - (R4) N parties should be able to construct M-of-N threshold PK (which is essentially *HD root key*)
 - (R5) Each of N owners should be able to derive tree-like hierarchy of PKs out of this M-of-N *HD root key*
 - (R6) To construct a signature M of N parties should sign in order to satisfy derived PK

## Existing implementation

Existing support for threshold signatures is limited.
Script language, Plutus, provides primitives to build script which will satisfy only **R1** (in the same way it's done by vanilla BTC multisigs).

None of **R2**-**R6** is provided or designed.

## Possible solutions


### BIP-45 approach

We maintain N HD wallet hierarchies, deriving keys with non-hardened derivation.

Requirement **R4** is satisfied by combining all N HD root PKs into a tuple, accompanied with `M`:

```
(root_pk_1, root_pk_2, .., root_pk_n, M)
```

This tuple is essentially a M-of-N threshold PK.

Each address should be a script address, redeemer script is one as for simple multisig addresses. PKs are picked at each hierarchy accordingly by same path. This answers to **R5**, also **R6** (because to spend such multisig you need to have access to M-of-N root secret keys).

This approach is one used in CoPay and doesn't require any sophiticated crypto (that said, all required crypto is in place already).


### More sophiticated schemes

We could use some less trivial threshold-signature schemes, for instance:

* Threshold-optimal DSA/ECDSA signatures ([paper](https://eprint.iacr.org/2016/013))
* Password-Protected Secret Sharing ([paper](https://eprint.iacr.org/2016/144))

Major concerns about these:

* Implementation complexity
* Support of HD wallets

  - Can BIP-32 be applied to scheme right away
  - Whether we have something similar (to some extent) to BIP-32-like HD wallets

Though potential benefits are:

* More flexible and elegant schemes
* Not showing information of M-of-N wallet structure (which is not a requirement though)
