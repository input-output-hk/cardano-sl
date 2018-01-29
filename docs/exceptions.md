# Guidelines for Exception Handling in Cardano SL

To determine the preferred way to handle exceptions in a piece of code, first
answer these questions:

1. Is this a programmer mistake or a regular error?
2. Is the code 100% pure or potentially impure?

Depending on the answers, an appropriate approach can be selected.

## Is this a programmer mistake or a regular error?

### Programmer mistake

When a particular codepath is not meant to be ever executed, but happens to be
executed anyway, this is a programmer mistake.

For instance, consider this piece of code:

```
-- Precondition (unchecked): input lists are the same length.
zipSameLen :: [a] -> [b] -> [(a, b)]
zipSameLen [] [] = []
zipSameLen (a:as) (b:bs) = (a,b) : zipSameLen as bs
zipSameLen _ _ = error "Lists of different length, precondition violated!"
```

The `zipSameLen` function is meant to work on two lists of the same length, and
calling it with lists of different length is a mistake on the programmer's part.
Code like `zipSameLen [] [1]` must be rejected at code review stage. However,
the function still must have the `zipSameLen _ _` case to have exhaustive
pattern matching.

A function that can be used erroneously (has a potential for programmer mistakes)
MUST have a comment that describes its preconditions. When it is called, the
call MUST have an explanation that assures why these preconditions hold.

For instance, when a programmer calls `Prelude.last xs`, he must add a comment
at this call site that explains why `xs` is guaranteed to be non-empty, even if
it seems obvious. A better option is to use functions and types that don't allow
for errors: instead of `Prelude.last` one can use `NonEmpty.last`.

In case of `zipSameLen` we could use length-indexed vectors:

```
zipSameLen :: Vec n a -> Vec n b -> Vec n (a, b)
```

It is always a tradeoff whether to allow programmer mistakes, or do type-level
trickery to avoid them. The decision process for this is out of scope of this
document.

### Regular error

Consider a function like `readFile`. It is entirely possible that the path
passed to it may point to a file that does not exist, and the programmer can't
do anything to prevent this. Existence of a file is not a property of our code,
it's the property of the outside world, and we have to deal with all possible
scenarios.

Another example is parsing user input. We might expect the user to enter a
number, but we have to consider the case that the user enters something else.

In cases when the erroneous scenarios are out of our control, we consider
these to be regular errors.

### Identifying error class

Can the execution of the error codepath be excluded by code review and static
verification (more precise types?)

* Yes: it is a programmer mistake
* No: it is a regular error

## Is the code 100% pure or potentially impure?

### Impure code

The code is considered impure when it's an `IO` action, a function that returns
an `IO` action, or similar. The code can be _potentially_ impure when it is
written in an abstract monad that can be instantiated to `IO`.

Definitely impure:

```
readFile :: FilePath -> IO String
```

Potentially impure:

```
lookupThrow :: MonadThrow m => Map k v -> k -> m v
```

### Pure code

Pure code is one that does not use `IO` operations. For example:

```
lookupMaybe :: Map k v -> k -> Maybe v
```

Notice that while it is conceivable that we have a `Map` of `IO` actions, and
instantiate `lookupMaybe` to `Map k (IO ()) -> k -> Maybe (IO ())`, the
instantiation does not inspect/use these `IO` actions, so it's not considered
potentially impure.

### Identifying code purity

Is there an instantiation of type parameters that would mean that there are
`IO` actions inspected/used in the code?

* Yes: it is potentially impure code
* No: it is 100% pure code

## Error handling practices

### Pure code, programmer mistakes

Do *not*:

* do this often, as if it's okay
* use `undefined`
* use non-exhaustive pattern matching

Do:

* try to use types to avoid the need in the first place
* comment extensively (invariants and precondition, reasoning)
* use the `error` function (or `impureThrow` with custom exception)
* use the `HasCallStack` feature

Before writing code that allows programmer mistakes, consult with colleagues how
it would be possible to prevent them statically. There might be tricks that
you're not aware of. Always make this trade-off conciously and responsibly.

(On the other hand, if static guarantees require GADTs or type families, perhaps
it's better to not overcomplicate code. Use your judgement.)

DISCUSSION: Should we create a synonym `bug = impureThrow` in Universum? This
would make the intention more clear.

### Pure code, regular errors

Do *not*:

* use `error` or `impureThrow`
* use `MonadFail`
* return `Either Text`

Do:

* return `Either ErrorADT`, `Maybe`
* wrap the underlying (pure!) monad in `ExceptT` or `CatchT`
* use `MonadError` or `MonadThrow` (methods of these classes). Note
  that if you define `f :: MonadError m => m ()`, it won't be pure

Consider parsing: it is pure, but we cannot make assumptions about the input. In
this case we might want to use `ExceptT ParseError`. Or consider a lookup in a
`Map`, where we don't know whether the key is present -- in this case we'd like
to return `Maybe v`. In 100% pure code, use one of these ways to handle errors:

* `Maybe`, `Either e`
* `MaybeT`, `ExceptT e`
* `CatchT`

Avoid using `Text` with the error message in place of `e` -- create a
proper ADT. In case creating a proper ADT feels too cumbersome, use
`CatchT`, which is equivalent to `ExceptT SomeException`. Note,
however, that using `SomeException` in pure code is not the best
practice, because the set of all possible exceptions is statically
known. Use it only if you are lazy to define yet another ADT.

Be careful not to use `MaybeT`, `ExceptT`, and `CatchT` in potentially impure
code. When in doubt whether the code is potentially impure, use `MonadThrow`.
(The reason we don't want `ExceptT` and co. in potentially impure code is that
they add additional exception mechanisms to the one that `IO` has, and
`catch`/`bracket` don't account for this).

### [Potentially] Impure code, regular errors

Do *not*:

* use `error` or `impureThrow`
* use `ExceptT`, `MaybeT`, or `CatchT`
* use `MonadError`
* use `throwIO`
* return `m (Either e a)` if `e` has `Exception` instance

Do:

* create a custom exception type
* use `throwM` (`MonadThrow`)

If you want to return `m (Either e a)` from a function, it's
recommended to define `instance TypeError "NOT AN EXC" => Exception e`
for your type `e`. If the meaning of `e` type is not related to
exceptional situations at all, it's not needed to define such
instance. But for example if `e` denotes a `ParseError`, please do
define it.

We disallow the use of `throwIO` only because it is redundant in the presence of
`throwM` and requires a stronger constraint (`MonadIO` rather than
`MonadThrow`).

Derive prisms for exception types with multiple constructors, so it's convenient
to use them with `catchJust`.

### [Potentially] Impure code, programmer mistakes

Use the same techniques as in pure code -- `error` or `impureThrow`. There are
two reasons for this:

* we would rather catch the error sooner than later, and `impureThrow` explodes
  when forced to WHNF, while `throwIO` or `throwM` explode when executed

* when in an abstract (but potentially impure) monad, using `throwM` might
  add an additional constraint

## Packages and modules

Do not import `Control.Exception` or `Control.Monad.Catch`! We use the
`safe-exceptions` to deal with asynchronous exceptions gracefully, so import
`Control.Exception.Safe`.

## Resource handling

Use `bracket` or to guarantee the release of resources. In case of concurrent
code, avoid `forkIO` or `forkProcess` in favor of the `async` package, as it
rethrows exceptions from the child threads. (Do not use the function `async`
itself when you can use `withAsync`, `race`, or `concurrently`).

When resource usage is non-linear, it's okay to use `ResourceT`, but
prefer `bracket` whenever possible. Non-linear resource usage is
anything that doesn't fit into the “allocate, use, deallocate”
pattern.


## Migration

We should identify the parts of the code that use `ExceptT` or
`MonadError` in impure or potentially impure code and replace them
with exceptions. If code can be made pure by replacing `MonadError`
with simple `Either`, we should do this replacement. For instance,
`mkMultiKeyDistr :: MonadError Text m => Map StakeholderId CoinPortion
-> m AddrStakeDistribution` becomes `mkMultiKeyDistr :: Map
StakeholderId CoinPortion -> Either Text AddrStakeDistribution`

We should locate all usages of `forkIO` and replace with appropriate functions
from `async`.

We should find where errors which are not programmer mistakes are thrown with
`error`, `undefined`, or `impureThrow`, and rewrite them to use correct error
handling method. This includes usages of partial functions, such as `read`.

We should find where errors are represented by `Text` and create dedicated data
types to represent them.

We should find places where preconditions/invariants are not reflected in the
comments, and add comments.

### Code references

This list is not exhaustive:

* `TxpGlobalVerifyMode`: https://github.com/input-output-hk/cardano-sl/blob/8507d03ba928e07daea57f9a52f6dc03a9d65779/txp/Pos/Txp/Settings/Global.hs#L37
  -- `MonadError` in potentially impure code
* `MonadRecoveryInfo`:
  https://github.com/input-output-hk/cardano-sl/blob/d598003ccde5d9848a11c54eec7542b299eb7c44/lib/src/Pos/Recovery/Instance.hs#L30
  -- `ExceptT` in impure code
* TBD

## Literature

The following documents were used to create these guidelines:

* https://www.schoolofhaskell.com/user/commercial/content/exceptions-best-practices
* https://github.com/fpco/safe-exceptions#readme
