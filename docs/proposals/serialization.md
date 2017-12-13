# Block retrieval optimization

This document aims to provide a detailed overview of a certain
development problem and suggest/compare several possible solutions. We
will mostly talk about _speeding up_ data
deserialization/verification and diffusion interface.

## Terms and definitions

Arbitrary datatype `T` construction/deserialization routine usually
consists of the following steps:

 * __(D1)__ Read bytes (from network or any type of storage)
 * __(D2)__ Check CBOR structure (bytestring is a correct CBOR encoded data)
 * __(D3)__ Check datatype canonicity
 * __(D4)__ Construct/deserialize datatype `T`
 * __(D5)__ Validate `T`.
   * __(D5.1)__ Validate parts of `T`.

Current codebase only provides interface to do all of it at
once. User runs D1 and feeds bytestring to decoder. Decoder performs
D2-D3, parses subfields and then constructs datatype (D4) with a
special constructor (D5).

It is currently impossible to perform (D4.1) and (D5.1), though it can
be implemented.

Now let's describe features that we'd like to have. We'll formulate
them in terms of use cases:
 * __(U1)__ Respond block requests quickly.
     * Only D1 is needed (just get those bytes and send them).
 * __(U2)__ Diffusion (as designed for Shelley) of blocks: receive block
   and distribute it on fly.
     * We need D4 + D5.1 (complete validation is not required). We don't
       want to have partial deserialization (though it's possible).
 * __(U3)__ Apply received block(s) to GState.
     * D1-D5 (complete procedure) are needed.
 * __(U4)__ Reading data (sometimes lots of it) from DB for different
   node local actions (LRC, GState checks, API for Middleware -- all
   of them require db traversing or LCA computations).
     * D1,D2,D4 needed. We can omit D3: we trust local database, 
       so let's turn off as much checks as it's possible.

## Solutions and comparison.

First of all it is needed to say that solving U1 is completely
orthogonal to solving U2-U4. We will just modify database typeclass so
it is possible to read raw data instead of already deserialized
value. So let's move to U2-U4.

Current state of CSL-1399 PR aims to cover U4 only, as it was
described in the YT task. It does it in a very compact and concise
way, though preventing us from implementing U2. Decoupling
(de)serialization and datatype verification would give us more
flexibility to solve U2. At least it seems to be. Before I move to
suggesting other solutions, I find it important to say that CSL-1399
PR idea can be applied to get rid of D2-D3 as it's required by
U4. Still, if we go with decoupling approach, it might be a good idea
to leave ReaderT (or patch cborg) to skip heavy D3. Let's call this
suggestion __Q1__ to refer to it later.

Let's have a closer look at the validation process itself. Currently
we have a bunch of types `T` that conform the following structure:
 1. Defined as `data T = UnsafeT { .. }`
 2. There is `mkT` function that works in the same constructor
    does. It defines several checks that define internal validity of
    `T`.
 3. We support invariant "every instance of type `T` is valid" by not
    using `UnsafeT` where it is possible to construct incorrect `T`.

Pros of this approach:
 * It was easy to implement (and it is concise).
 * It integrates into decoder nicely so we shouldn't ever think that
   `T`'s subfield `S` can be invalid. If it's `decode`d, it's
   instantiated. Then it's valid. We can simply feed `S` to `makeT`.
 * Aligns with Q1 pretty well.

Cons of this approach is that it, well, combines verification and
deserialization, which we want to avoid.

The first dichotomy i would like to highlight is:
 * __(Q2)__ is it alright to break invariant (3) (see 2 paragraphs
   before)? Rephrasing: may we assume that type `T` can be valid to
   different degree? Or we should create `Ti` for each validation
   step? (like `T0` -- no checks enforced, `T1` -- some checks
   enforced, ... `Tn` -- valid). If yes, which `n` should we take?

We need to figure out whether it's alright to accept Q2, it's a tough
tradeoff: if we reject Q2 it can lead to massive code duplication and
unnecessary overabstraction.

There's another issue with `T` verification. Imagine it's now
completely separate and we already have `T` deserialized, but not
verified (`T` or `T0` according to `Q1`). How would `verifyT` look
like? Well, you should consider the fact that now you're obliged to
perform all the subfields checks by yourself. Hopefully, this will be
easy to maintain. Just a notice.

I claim that if we're aiming to solve U2 then the only thing we
need to agree on is Q2: U1 is irrelevant, U4 is solved by
deserializing to unsafe `T` or `T0` (and maybe skipping D2 D3 as
suggested by Q1).

### Solution 1: Approach with "RawX and X".

Described in https://github.com/input-output-hk/cardano-sl/pull/1823#issuecomment-342475355

It solves the problem by rejecting Q2. Though, there is a serious
drawback with it: You must propagate "Raw" to all record
fields. Imagine you have the following datatype set

```
data C = C { ... }
data B = B { bC :: C, ... }
data A = A { aB :: B, ... }
```

Now imagine you want to turn off validation of C and A. It is
impossible to do it without propagating "Raw" to B:

```
data C = C { ... }
data RawC = RawC { ... }

data B = B { bC :: C, ... }
data RawB = RawB { bC :: RawC, ... }

data A = A { aB :: B, ... }
data RawA = A { aB :: RawB, ... }
```

You now should see where it is going. Imagine you want to have
`RawB1` (which doesn't check everything) and `RawB2` (which checks
only `bC`, but not, say, `bD`): how would you implement it without
duplicating datatypes?

### Solution 2: using GADT/DataKinds

Another no-Q2 solution suggested in
https://github.com/input-output-hk/cardano-sl/pull/1823#issuecomment-342478781

Has the very same drawback as the first solution. It is actually a
cosmetic improvement that gives another tradeoff:
1. Now you can't mistakenly define `RawA` and `A` in different
   modules (which feels like chaos).
2. But now you should use this parameter `v` everywhere you use block.

### Solution 3: phantom types

Same as 3, but instead of having two constructors you now have:

```
data Block (v :: Verification) = Block {..}
```

This doesn't have the (bad) possibility to have different
implementations of block for verified and unverified parameter.

### Solution 4: watching your code.

Q2-accepting solution that works essentially as:
1. Use `T`, but use implicit function invariants (e.g. "this function
   works on valid `T` only").
2. Use `verifyT` or partial verifications.

The main idea behind the confidence it will work is that most of the
CSL code/core uses valid block already and unsafe blocks will be used
in the diffusion layer only, so these module sets have minimum number
of intersections thus minimizing ability to make a mistake too.

### Solution 5: `Verified a` newtype

Introduce newtype:
```
data Fully

newtype Verified level s = Verified s
```

Then rewrite definitions in the following way:

```
-- old:
applyBlock :: GState -> Block -> GState

-- new:
applyBlock :: GState -> Verified Fully Block -> GState
```

For type `T` you'll need to define lenses for `Verified a T` (all
lenses applicable to `T`). To construct `Verified Fully T` you'll need
to do full verification and/or serialization with all checks. It is
much more verbose than using phantom types, though it is not obvious
whether using this approach will produce bigger diff.

## Decision

Decision was made in the following PR:
https://github.com/input-output-hk/cardano-sl/pull/1903

We will adopt solution 3 (phantom types) and extend it with type families (as 
suggested in the PR) if needed.
