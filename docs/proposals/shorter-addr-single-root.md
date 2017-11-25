# Address from single Merkle root

| Workstream | Author(s) |
| ---- | ----- |
| Short addresses | George Agapov |


In this document we analyze possibility of forming address from single Merkle root.

This idea was previously discussed in meetings involving Aggelos, Duncan and being quite desirable to implement, encounters several pitfalls being applied directly as is.
This document's purpose is to define this pitfalls and look for possible solutions.

Document is structured as follows:

  * [Terminology](#terminology)
  * [Address structure problem definition](#address-structure-problem-definition)
    + [Constraints](#constraints)
  * [Current solution for address structure](#current-solution-for-address-structure)
  * [Solution with single hash](#solution-with-single-hash)
    + [Direct application](#direct-application)
    + [Using reference to blockchain](#using-reference-to-blockchain)
      - [Solving D4](#solving-d4)
  * [Problem of reliable data aggregation](#problem-of-reliable-data-aggregation)
    + [Solution 1: use OR](#solution-1--use-or)
    + [Solution 2: using aggregate signatures in crazy way](#solution-2--using-aggregate-signatures-in-crazy-way)
    + [Other solutions](#other-solutions)

## Terminology

We use:

- *SK* to refer to secret key as defined in Ed25519 standard
- *PK* to refer to public key as defined in Ed25519 standard

## Address structure problem definition

Cardano SL address is piece of data containing exhaustive information of destination to which one can send transaction.

For CSL this destination is defined two-fold:
* Balance key (key to control balance)
* Stake key (key to control stake)

For more information on balance and stake, please refer to [page on Cardano Docs](https://cardanodocs.com/cardano/balance-and-stake).

Additionally, destination may contain some auxiliary information useful for fund receiver
to do his book keeping. Nowadays it's single piece of data (but later it might be extended):
* HD wallets derivation path

So, CSL address can be represented as tuple `Addr = (B, S, A)` where
 * `B` is balance key reference (e.g. hash of PK for balance)
 * `S` is stake key reference
 * `A` is auxilary information for fund receiver (book keeping)

Problem of address structure: how we store this tuple on blockchain to satisfy **Constraints** (defined below).

### Constraints

We describe constraints as they're given by design of rest parts of CSL (assuming it's fixed as for Byron release).

Hard constraints (which we *must* satisfy):

* (H1) Given `Addr` (constructed from `B`), one shouldn't be able to derive PK for balance key (to which `B` relates)
* (H2) Given `Addr` (constructed from `S`), one shouldn't be able to derive SK for stake key (to which `S` relates)
* (H3) Given `Addr` (constructed from `S`, `A`) one shouldn't be able to construct `Addr' = (B, S', A')` with `S' /= S`
* (H4) Given `Addr` (constructed from `S`, `A`) and current state of blockchain, one should be able to derive `S`, `A`
* (H5) Given `Addr` (constructed from `B`), `B` and current state of blokchain, one should be able to proove `B` belongs to address `Addr`

Desirable constraints (which we *could* have):

* (D1) Given `Addr` (constructed from `S`, `A`) one shouldn't be able to construct `Addr' = (B, S', A')` with `A' /= A`
* (D2) Given `Addr` (constructed from `S`) one can proove that `S` belongs to address `Addr`
* (D3) Given `Addr` (constructed from `A`) one can proove that `A` belongs to address `Addr`
* (D4) Given `Addr` (constructed from `S`, `A`) one can proove that pair `(S, A)` belongs to address `Addr`
* (D5) Resulting `Addr` should be represented to user in not more than 55 chars
   * Number 55 is arbitrary, but it shouldn't be much larger than address of Bitcoin which is 34 character long
   * 55 characters roughly corresponds to 40 bytes (given address is represented in base58)

## Current solution for address structure

Please read section [Address structure on Cardano Docs](https://cardanodocs.com/cardano/addresses/#address-structure) to get understanding on current solution.

In short, address is represented on chain (and to user) as tuple `Addr1 = (Addr1_root, S_pubkeyhash, Aux)`, where:
 * `Addr1_root = blake2b_224 (B_pubkeyhash, S_pubkeyhash, Aux)`
 * `blake2b_224` is particular hash function
 * `Aux` is some representation of `A`

Here we only analyze it:

- It solves:
  - *H1* by using hashing
  - *H2* by using hash of stake PK
  - *H4* by storing tuple with `S`, `A` explicitly represented
  - *H5*, *H3*, *D1* by using Merkle-tree-like root `Addr1_root` in address and not exposing `B`
* It can not solve *D2*-*D5*

## Solution with single hash

Idea circulating in various talks is to have address stored not as tuple `Addr1`, but as single `Addr1_root`.

### Direct application

What will change if one applies this idea directly.
I.e. having `Addr2 = Addr1_root = blake2b_224 (B_pubkeyhash, S_pubkeyhash, Aux)`:
 - *H4* is not solved
 - *H5* is not solved unless `S`, `A` are somehow provided
 - *D5* is solved
 - *H1*-*H3*, *D1* are still solved
 - *D2*, *D3*, *D4* are still not solved


### Using reference to blockchain

Let's now assume we can store some part of `(S, A)` on blockchain and pack it into 10b reference. For instance:
* 4b reference for `S`
   * Take first 4b of PK hash
* 6b to pack `A` (only HD wallet derivation path nowadays)
   * Doable for most addresses if we revisit index generation algorithm, encoding, also utilize fact that `S` usually has 1-to-1 correspondance with HD wallet root (hence no need for crypto tag)

Such a short reference to `S` is certainly unreliable, but given blokchain we can determine list of cadidates `S_candidates` for such `S` which will assumably be negligibly small.

Let's assume by some magic we can construct `Addr3_root` in such way that it satisfies *D4* and weighs no more than 32b

Then we can construct `Addr3 = (Addr3_root, S_ref_4b, A_encoded_6b)` which will weigh no more than 42b which is close to target.

Given the list `S_candidates` is not to big, we test `(S, A)` to be part of `Addr3_root` for each `S` from `S_candidates`.
If one is found, then `Addr3` is considered valid and `S` is stake key reference.

#### Solving D4

The most interesting part in the story is solving *D4* by constructing some piece of data `Addr3_root`.

We defer it to [separate section below](#problem-of-reliable-data-aggregation).

## Problem of reliable data aggregation

Disclaimer: author of the section is not well educated in cryptography and is not aware which name
this problem is assigned in scientific literature. Will be happy to change title, definitions, descriptions when somebody provides feedback.

Suppose we have two pieces of data: `A` and `B` with no particular structure (say, both `A` and `B` are cryptographic hashes of some arbitrary data).

We want to define two functions:
 * `Aggregate (A, B)`, returning some arbitrary piece of data
 * `Verify_aggregated (C, A, B)`, returning boolean value

Designed in such a way that:
* Both `A`, `B` are not more than 32b
* `Verify_aggregated (C, A, B) <-> C == Aggregate (A, B)`
  I.e. you can combine two pieces of data `A` and `B` in such `C` that allows you to further verify `A`, `B` where used in `C` construction.
* Size of `C` is less than 32 bytes

Obviously, definition as above is not good, in particular because `A`, `B` are in sum 64b whereas `C` is limited by 32b. So similar to definition of hash function, we should account for collisions here. But as was said, I'm rather looking for existing well-defined problems might look similar to above than clearly defining my own.

What are possible solutions to the problem (with no argument on colisions etc.)?

### Solution 1: use OR

Obvious solution to above is using OR. I.e.
* `Aggregate (A, B) = A || B`
* `Verify_aggregated (C, A, B) = A && C && B`

As was mentioned above, we don't provide any reasoning how good and reliable is this solution (probably bad).

### Solution 2: using aggregate signatures in crazy way

Aggregate signature is signature scheme that supports aggregation: Given n signatures on n messages from n users, it is possible to aggregate all these signatures into a single signature whose size is constant in the number of users, messages. This single signature will convince the verifier that the n users did indeed sign the n original messages.

We can utilize this primitive in following way:
* Assume SK `Z` is publicly known
* We sign `A`, `B` with `Z`, aggregate two signatures into single one, `C` => we have `Aggregate`
* We validate `Z` indeed signed `A`, `B` by checking signature `C` that it is indeed signature of `A`, `B` made by `Z`

Also, no reasoning of how good is the approach. But given size of signature for eliptic curve schemes, we can fit into 32 bytes for `C`.

### Other solutions

It's almost certain there exist more straightforward solutions and this document is to be extended with such.
