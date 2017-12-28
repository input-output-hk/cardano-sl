# Guidelines for Effect Handling in Cardano SL

A monad comes with two basic operations, ``return`` and ``>>=``, but this
interface is barely useful on its own. Monadic effects extend the ``Monad``
interface with additional operations (such as ``ask``, ``local``, ``get``,
``put``, ``throwError``, ``liftIO``, ``tell``, etc).

Besides general purpose effects (such as reader/writer/state) it is customary to
define our own, domain-specific effects, needed by the application (slotting,
gstate, database access, etc).

## Method dictionaries

A _method dictionary_ for an effect is a record (a product type with named
fields) that contains operations on a monad. For example:

```
data Logging m =
  Logging
    { _logError :: String -> m (),
      _logDebug :: String -> m ()
    }
```

Any MTL-style class can be represented as a method dictionary. To better
understand the relationship between classes and records, read the [Scrap your
type classes](http://www.haskellforall.com/2012/05/scrap-your-type-classes.html)
article.

## Instrinsic effects and capabilities

We classify the effects into two categories:

* _intrinsic effects_ provide operations enabled by the additional structure of
  the monad itself. For example, the intrinsic effect of `StateT` is adding
  state to a computation, `ExceptT` adds alternative exit path, `ConduitM` adds
  streaming, and so on. intrinsic to `[]` and `ConduitM`, etc.

* _capabilities_ are effects that can be added to the monad by passing more
  context to the methods (manually or via `ReaderT`). For instance, a capability
  with database access could be represented as a method dictionary with DB
  methods or as a handle to a DB session.

Implementations of capabilities may rely on intrinsic effects of the base monad.
For example, a database access capability might need to perform `IO` operations.

We could represent capabilities as intrinsic effects if we create monad
transformers for them:

```
data DatabaseT m a = DatabaseT { runDatabaseT :: DBHandle -> m a }
```

These transformers would be always isomorphic to `ReaderT`. On the other hand,
we cannot reduce instrinsic effects to capabilities.

We represent all effects as capabilities whenever possible, and as intrinsic
effects otherwise. For example, to add mutable state to a computation, we have
two options:

* use the intrinsic effect provided by `StateT`
* use the intrinsic effect provided by `IO` and a capability that stores an
  `IORef`

In case we are writing pure code, we opt to use `StateT`, but if we already have
`IO` in our monadic stack, we use a capability with an `IORef`.

For example, to use mutable state in `IO`, do not add a
transformer that adds another intrinsic effect (`StateT`), but reuse the
existing `IO` base monad and represent the mutable state by passing an `IORef`
in the `ReaderT` context.

### Identifying effect class

Can the effect be reduced to a function argument or a `ReaderT`-isomorphic
transformer?

* Yes: this is a capability
* No: this is an intrinsic effect

## Pure code

We write pure code using concrete monad transformers (`StateT`, `ExceptT`) and a
concrete base monad (`Identity`, `ST`) as opposed to MTL-style constraints
(`MonadState`, `MonadError`) on an abstract monad:

* Do: `ExceptT e (State s) a`
* Do not: `(MonadState s m, MonadError e m) => m a`

(NB. we still use the methods from MTL-style classes to avoid lifting)

There are several reasons for using concrete monads:

* this highlights the purity of the code – we know that the abstract monad will
  not be instantiated to `IO`

* this enables GHC to optimize the code more aggressively – a recursive
  computation in the `State` monad can be compiled to a tight loop in machine
  code (not too different from what a C++ compiler would produce), while an
  abstract monad with a `MonadState` constraint inhabits inlining and can make
  GHC produce dictionary-passing code

* it is easier to reason about the interactions between effects – for instance,
  `StateT` over `ExceptT` has different backtracking behavior from `ExceptT`
  over `StateT`, and the computation might rely on it

We do not use `WriterT` because it leaks space, but it can often be replaced
with `StateT`:

* Do: `StateT w`, `modify (mappend m)`
* Do not: `WriterT w`, `tell m`

(NB. there are legitimate use cases for `WriterT`, but they involve `listen` and
`pass`)

We do not use nested monad transformers unless necessary:

* Do: `StateT (s1, s2)`
* Do not: `StateT s1 (StateT s2 ...)`

(NB. it is possible to use something like `StateT s1 (ExceptT e (StateT s2))` to
have different backtracking behavior for `s1` and `s2`)

## Impure code

Impure code has access to `IO`. We use the following monad to
write (almost) all impure code:

```
m :: (Has A ctx, HasLens B ctx) => ReaderT ctx Base
```

We can add more monad transformers when we need other intrinsic effects:
`ConduitM`, `ResourceT`, etc, but this is an exception to the rule. Normally, we
do not have any monad transformers besides `ReaderT`.

Do: `ReaderT ctx Base`
Do not: `MonadReader ctx m`, `MonadIO m`, etc

### The `Base` monad

The `Base` monad is defined as follows:

```
newtype Base a = WrapBaseIO (IO a)
```

We intentionally do not define the `MonadIO`, `MonadBase IO`, and
`MonadBaseControl IO` instances for `Base`, so that `IO` actions are explicitly
annotated with `WrapBaseIO`. This is done to discourage doing `IO` directly in
favor of granular, capability-based effect handling.

### The `Has` constraints

The `Has` and `HasLens` classes are defined as follows:

```
class Has a ctx where
  get :: Getter ctx a

instance Has a ctx => HasLens a ctx where
  lensOf :: Lens' ctx a
```

This means that we can use `view` (from the `lens` package) to access parts of
the `ReaderT` context:

```
m :: (Has A ctx, HasLens B ctx) => ReaderT ctx Base
m = do
  a <- view $ get @A
  x <- view $ lensOf @B . bPart
  local (lensOf @B %~ f) $ do
    b <- view $ get @B
    ...
  magnify (get @A . aPart) $ do
    ...
```

The `Has` constraints is used to provide read-only access, while `HasLens`
can be used to modify the context locally.

### Capability definitions

We define capabilities instantiated to the `Base` monad, but then additional
helpers to use the capability in the `ReaderT` monad:

```
data Logging =
  Logging
    { _logError :: LoggingParams -> String -> Base (),
      _logDebug :: LoggingParams -> String -> Base ()
    }

logError :: (Has LoggingParams ctx, Has Logging ctx) => String -> ReaderT ctx Base
logError msg = do
  l <- view $ get @Logging
  lp <- view $ get @LoggingParams
  lift $ _logError l lp msg

logDebug :: (Has LoggingParams ctx, Has Logging ctx) => String -> ReaderT ctx Base
logDebug msg = do
  l <- view $ get @Logging
  lp <- view $ get @LoggingParams
  lift $ _logDebug l lp msg
```

The reason we avoid the `m` or `ctx` parameter in the capability definition is
to allow changing `m` and `ctx` without adjusting the capabilities stored in it
(this is needed for context extension, for example).

### Context extension

Assume we have a function `f` that requires a bigger context than a function
`g`:

```
f :: (Has A ctx, Has B ctx, Has C ctx) => ReaderT ctx Base
g :: (Has A ctx, Has B ctx) => ReaderT ctx Base
```

When we want to call `f` from `g`, we need to construct `C` and extend the
context with it. In this case, we have to create an extension wrapper:

```
data ExtC ctx = ExtB { ecC :: C, ecInnerCtx :: ctx }
makeLensesWith postfixLFields ''ExtC
```

And then we use `withReaderT` to extend the context:

```
g = do
  c <- conjureUpC
  withReaderT (ExtC c) f
```

#### `Has` instances for context extension

For this to work, we need to have relevant `Has` instances
for `ExtC`:

```
instance Has A ctx => Has A (ExtC ctx) where
  get = ecInnerCtx_L . get @A

instance Has B ctx => Has B (ExtC ctx) where
  get = ecInnerCtx_L . get @B

instance Has C ctx where
  get = ecC_L
```

To automate writing `Has` instances inherited from the inner context, we have a
default definition for `get`. The actual code should look like this:

```
-- no method definitions, using defaults
instance Has A ctx => Has A (ExtC ctx)
instance Has B ctx => Has B (ExtC ctx)

-- access to the inner context
instance HasInnerContext (ExtC ctx) where
  type InnerContext (ExtC ctx) = ctx
  getInnerContext = ecInnerCtx_L
```

#### Override avoidance

When doing context extension, be careful with overrides. It is possible to
extend the context in a way that would overshadow a value of the same type
in the context:

```
data BaseContext = BaseContext A B C

main = do
    a <- initA
    b <- initB
    c <- initC
	runReaderT (BaseContext a b c) $ do
      g -- (1)
      f

f :: (Has A ctx, Has B ctx) => ReaderT ctx Base ()
f = do
  newC <- conjureUpC
  withReaderT (ExtC newC) $ do
    g -- (2)

g :: (HasA ctx, HasB ctx, HasC ctx) => ReaderT ctx Base ()
```

The (1) call of `g` has access to `c` from the base context, but the (2) call of
`g` has access to `newC` instead. Basically, extension allows shadowing of
context parts.

The trouble is that sometimes there is an assumption of coherence in the code.
For example, consider `A ~ Networking`, `B ~ Database`, `C ~ Logging`
(capabilities represented by method dictionaries). When we construct the
networking and database capabilities, we may capture the logging capability in
their closures. This means that in (1), all three capabilities will be using the
same logging method. However, in (2) we override the logging capability with
`ExtC`, but do not update the closures, so there will be two logging methods in
use.

## Potentially impure code

These is code that can be instantiated to be either pure or impure:

```
f :: MonadThrow m => m a

fPure :: Maybe a
fPure = f

fImpure :: IO a
fImpure = f
```

While our guidelines state to avoid abstract monads, there is one case when
we allow this style of code: for general-purpose helpers.

Domain-specific code must be either in a concrete pure monad, or in `ReaderT ctx
Base`.
