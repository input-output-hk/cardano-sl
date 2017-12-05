===============================
 Monadic effects in Cardano SL
===============================

A monad comes with two basic operations, ``return`` and ``>>=``, but this
interface is barely useful on its own. Monadic effects extend the ``Monad``
interface with additional operations (such as ``ask``, ``local``, ``get``,
``put``, ``throwError``, ``liftIO``, ``tell``, etc).

Besides general purpose effects (such as reader/writer/state) it is customary to
define our own, domain-specific effects, needed by the application (slotting,
gstate, database access, etc).

Sure enough, we don't need those effects in isolation: we want to use multiple
effects simultaneously. And that's where it gets difficult: as it turns out,
combining effects is a huge design space, and there are many conflicting
approaches (concrete transformer stacks, mtl-style classes, extensible effects,
and what not).

What further complicates matters is that the choice of the approach to monadic
effects will affect the entire codebase in drastic ways, since the effects are
so ubiqutous. This means that it's impossible to quickly try each possible
approach, one must rewrite half the application to do so.

History
-------

In Cardano SL we've used multiple approaches to effects, none of them
satisfactory. The goal of this document is to provide a reference point for
discussion about further direction of travel.

MTL-style Transformers and Classes
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The first approach to effects that was used in Cardano SL was simple: for each
effect, create a class with operations and a monad transformer that implements
these operations::

    class MonadFoo m where
        ...

    data FooT m a = FooT { runFooT :: ... }

    instance MonadFoo (FooT m)
    instance MonadFoo m => MonadFoo (ReaderT r m)
    instance MonadFoo m => MonadFoo (StateT s m)
    instance MonadFoo m => MonadFoo (ExceptT e m)
    ...

    class MonadBar m where
        ...

    data BarT m a = BarT { runBarT :: ... }

    instance MonadBar (BarT m)
    instance MonadBar m => MonadBar (ReaderT r m)
    instance MonadBar m => MonadBar (StateT s m)
    instance MonadBar m => MonadBar (ExceptT e m)
    ...

This approach mirrors the one of ``mtl``. However, ``mtl`` is concerned with
general purpose effects, of which there are only a few (under 10, even if we
consider more exotic ones like reverse state or logic-t), whereas
domain-specific effects are numerous. Besides, general purpose effects don't pop
up left and right, whereas domain-specific ones might be created/deleted
depending on how the application evolves. All in all, this made mirroring
``mtl`` quite a bad idea.

Pros:

* Simple and familiar. It's easy to reason about effects when there's a 1-1
  correspondence between classes and transformers. Virtually everyone is
  familiar with ``mtl`` and can quickly learn to apply this approach.

* Flexibility. Given a monad ``m``, one can easily add an effect to it by
  putting the relevant transformer on top. It's convenient to use effects
  locally, not only application-wide.

Cons:

* Bad run-time performance. Having a transformer per effect means no flattening.
  This boils down to ``ReaderT (a, b, c, d) m`` being more performant than
  ``ReaderT a (ReaderT b (ReaderT c (ReaderT d m)))`` (same goes for ``StateT``,
  ``WriterT``, ``ExceptT``, etc). Since transformers for many domain-specific
  effects are isomorphic to one of these general purpose effects, having a dozen
  of nested transformers isn't such a good idea. (@int-index has benchmarks to back up
  this claim).

* No run-time configurability. The stack of transformers determines at
  compile-time what effect implementations are used. This means that if we
  wanted to select an implementation at run-time (via a CLI flag, for instance),
  we had to use a different stack of transformers. The amount of possible
  combinations grows rather quickly. For instance, if feature X has ``2``
  implementations and feature Y has ``3``, then we need ``2*3=6`` transformer
  stacks. (As discovered later, this can be addressed with closed effect sums,
  but then we lose extensibility, so it's still an issue).

* Cost of introducing new effects. For ``N`` transformers and ``M`` classes,
  there must be ``N * M - k`` instances (where ``k`` is the amount of unneeded
  or impossible effect combinations: one does not simply use ``WriterT`` with
  ``ContT``). The more effects we have in the application, the more costly it is
  to add one more (at 10 effects we already hit 100 instances, the cost is
  insurmountable). Not only this is boilerplate-heavy, it's also antimodular, as
  every effect needs to know about each other.

Conclusion:

* In retrospect, we could probably solve the ``N*M`` issue by careful use of
  ``{-# OVERLAPPABLE #-}`` instances (although ``Mockable``-related type
  families would still lead to a fairly large amount of boilerplate).
  Nevertheless, the run-time performance of highly nested transformer stacks
  might be unsatisfactory. (Slowdown is linear in the amount of layers).


Using Ether
~~~~~~~~~~~

The core observation is that many domain-specific effects are isomorphic to
general purpose ones. The majority of monad transformers in Cardano SL were
basically clones of ``ReaderT`` or ``StateT``. This is why we decided to use
``ether``: unlike ``mtl``, it allows using multiple transformers of the same
sort (i.e. multiple ``ReaderT``) in the same transformer stack, so we could
simply replace our hand-crafted transformers with those from ``ether`` and put
an end to the ``N*M`` problem. Furthermore, ``ether`` offers *flattening*,
solving the run-time performance issue as well.

Typically, the code would look like this::

    type MonadFoo = Ether.MonadReader Foo
    type FooT = Ether.ReaderT Foo

    type MonadBar = Ether.MonadReader Bar
    type BarT = Ether.ReaderT Bar

Even when we needed a custom class with methods (rather than a synonym for
``Ether.MonadReader``), we could define a single instance for ``IdentityT``, no
boilerplate required.

We used flattening to achieve good run-time performance, but kept many custom
classes with effect operations (to allow for varying implementations). This led
to a peculiar situation where the transformer stack consisted mostly of
``IdentityT``::

    type M =
        TaggedTrans FooEff IdentityT $
        TaggedTrans BarEff IdentityT $
        TaggedTrans BazEff IdentityT $
        ReaderT (FooEnv, BarEnv, BazEnv) IO

Pros:

* Extensibility. Introducing a new effect is really cheap. The code is modular
  and effects don't need to know about each other.

* Flexibility. (Same as above, local use of effects).

* Good run-time performance. Since in the end the entire monad transformer stack
  was just ``ReaderT`` with a bunch of ``IdentityT`` on top (and occasional
  local ``StateT``), we enjoyed good run-time performance.

* Conciseness. No boilerplate.

Cons:

* Bad run-time configurability. (Same as above)

* Bad compile-time performance. Due to the way flattening works in Ether and due
  to a GHC bug, the compile-time performance was devastating. Turning ``-O2``
  could mean hours of compilation and required up to 65 GIGABYTES of RAM
  (ridiculous!). This was because GHC generated an exponential amount of
  coercions (as evidenced by investigating .hi-files). Basically, @int-index no longer
  can recommend Ether to people as there is no good solution to this known.

Conclusion:

* Migration to Ether allowed us to remove an immense amount of boilerplate,
  modularize the code, and get good run-time performance. However, lack of
  run-time configuability was quite inconvenient, and bad compile-time
  performance marked this approach a no-go.

Current Situation
-----------------

After we've realized what led to bad compile-time performance, @int-index came
up with an idea of ``ExecMode``. Basically, we continued to use classes from
``Ether``, but rather than having numerous ``IdentityT`` layers there was a
single ``newtype`` wrapper around ``ReaderT ModeEnv Production`` at the bottom.
This solved the compile-time performance issue completely at the cost of a
moderate increase in boilerplate.

However, FPComplete began to see ``ether`` as a suboptimal solution, and asked
us to purge its remains. Now we were supposed to remove all our custom classes,
replacing them with method records. Those records would go into a ``ReaderT``
and be passed everywhere manually (as opposed to instance search). Instead of
distinct ``Ether.MonadReader`` constraints, now we had to use ``MonadReader
ctx``, passing an annoying ``ctx`` parameter everywhere, and placing constraints
on it. The final transformer stack is just ``ReaderT ModeCtx Production``, and
not even a newtype on top.

Technically, we're still in the process of migration, as we haven't removed all
of our custom classes yet. Just to clarify: in this section we'll discuss the
current transitional state, and it's more painful than what was actually
proposed by FPComplete.

In other words, there is a compliсation that actually we have several approaches
combined:

* General approach a la ExecMode
* Mockable-based constraints
* Reflection-based constants (used instead of MonadReader for constant data within single execution)
* Method dictionaries (for supporting SendActions, a primitive from networking)
* ReaderT with method dictionaries

General Approach
~~~~~~~~~~~~~~~~

Now our code follows this pattern::

    -- effect definitions

    class MonadBaz
        baz :: ...

    defaultBaz = ...

    class MonadZaz
        zaz :: ...

    defaultZaz = ...


    -- mode definitions

    data QuuxCtx = ...

    type QuuxMode = ReaderT QuuxCtx Production

    instance HasFoo ModeCtx
    instance HasBar ModeCtx

    instance MonadBaz QuuxMode
        baz = defaultBaz

    instance MonadZaz QuuxMode
        zaz = defaultZaz


Pros:

* Good compile-time performance. There's only a single layer of transformers,
  instance search is quick, no coercions involved.

Cons:

* Bad run-time configurability. (Same as above)

* Boilerplate. Various ``HasFoo`` instances with field lenses, ``MonadBaz``
  instances to choose method implementations for the current mode. There's also
  that annoying ``ctx`` parameter.

* Cost of introducing new modes. The approach is inflexible, as introducing a
  new mode has an extremely high cost (due to boilerplate). Assuming we want to
  avoid nested ``ReaderT``, adding one more field to the context requires a new
  mode.

* Lack of inheritance. It's hard to define one mode in terms of another, with
  only minor changes. Either it becomes hard to maintain consistency, or it
  becomes hard to do overrides (as happened to ``AuxxMode``).

* Volatile ``runReaderT``. It's difficult to reason about code when
  ``runReaderT`` might imply something besides supplyng the value of the
  ``ReaderT`` environment (handling ``MonadReader``). The situation arises
  because we define instances for ``ReaderT`` without a newtype (and sometimes
  even with ``{-# OVERLAPPING #-}``).

Conclusion:

* Current solution requires huge swaths of boilerplate code, it's hard to
  reason about the code, and it's inflexible. We must seek other options.

Usage of ``reflection``
~~~~~~~~~~~~~~~~~~~~~~~

To pass constant configuration to application components, instead of
``ReaderT`` we use ``reflection``. More specifically, we use the dumb and unsafe
``Given``-style reflection, avoiding the type-level complications of proper
``Reifies``-style reflection. It turned out to be a great design choice: we've
cut the potential amount of custom classes greatly, and the configs are
available even in class instances (such as ``Bi``).

Usage of ``Mockable``
~~~~~~~~~~~~~~~~~~~~~

Certain properties of the base monad are expressed using ``Mockable`` from
``time-warp-nt`` rather than custom classes.

The ``Mockable`` class is defined like so::

    class Monad m => Mockable d m where
        liftMockable :: d m t -> m t

And here is an example of ``d``::

    data Async m t where
        Async :: m t -> Async m (Promise m t)
        WithAsync :: m t -> (Promise m t -> m r) -> Async m r
        Wait :: Promise m t -> Async m t
        WaitAny :: [Promise m t] -> Async m (Promise m t, t)
        CancelWith :: Exception e => Promise m t -> e -> Async m ()
        AsyncThreadId :: Promise m t -> Async m (ThreadId m)
        Race :: m t -> m r -> Async m (Either t r)
        Link :: Promise m t -> Async m ()

Basically, having ``Mockable Async`` is nearly the same as having a custom class
``MonadAsync``, but there are some differences.

There's an increase in boilerplate at effect definition site, as one needs to
define helper methods for each ``Mockable`` method::

   {-# INLINE async #-}
   async :: ( Mockable Async m ) => m t -> m (Promise m t)
   async m = liftMockable $ Async m

   {-# INLINE withAsync #-}
   withAsync :: ( Mockable Async m ) => m t -> (Promise m t -> m r) -> m r
   withAsync mterm k = liftMockable $ WithAsync mterm k

   {-# INLINE wait #-}
   wait :: ( Mockable Async m ) => Promise m t -> m t
   wait promise = liftMockable $ Wait promise

   ...

This increase is linear in the amount of supported methods.

On the other hand, there's a decrease in the amount of boilerplate (linear in
the amount of transformers), as each transformer needs to define
``liftMockable`` just once.


Method Dictionaries
~~~~~~~~~~~~~~~~~~~

``SendActions`` is another effect which is implemented differently from everything
else in our system. It's an explicitly passed method dictionary that has the
monad ``m`` as a type parameter. Unlike classes, explicit dictionaries must be passed
manually (not by compiler via instance search), which might be both an advantage
and a disadvantage.

``SendActions`` is a perfect example of a *local* effect: in general, we don't
have the ability to send requests, but we get this ability when we start a so
called conversation. Doing a local effect via an explicit dictionary is a
testament to poor support for local effects of our current approach with modes.

One of the problems with ``SendActions`` is its use of natural transformations. We
have a helper ``hoistSendActions``, and right now we horribly misuse it. Since
``m`` in ``SendActions m`` is in an invariant position, we need
``hoistSendActions`` to convert ``SendActions n`` to ``SendActions m``, and it
requires two natural transformations: ``n ~> m`` and ``m ~> n``.

First, providing these natural transformations is an inconvenience (the ``n ~> m`` one
tends to be a mere lift, but the second one must do certain tricks to unlift).
Second, it introduces a potential for horrible, subtle bugs. As was said, right
now we misuse this helper, in particular because we call it in a runner,
``runRealMode``; therefore, the unlift natural transformation must reconstruct the
monadic context from what it has at transformer stack initialization site, not
at conversation fork site; therefore, any local modifications to the monad
transformer stack are *not* reflected in forked conversations (such as ``ReaderT``
local).

To put it into concrete terms, here is a code demonstration::

    x <- ask
    local f $ enqueueMsg msg $ \_ _ -> (:|[]) $ do
      y <- ask

What do you think, is ``y`` equal to ``x`` or ``f x``? The way we currently use
``hoistSendActions``, it will be equal to ``x`` (completely oblivious to ``f``). This
might be not at all what a programmer would expect.

The moral of this story is that, perhaps, explicit dictionaries are a bad design
because it's very easy to misuse them. A good effect system should take care of
things like this. (But perhaps it's an over-genralization and it's only bad to unlift,
whereas lifting is straightforward).

Usage of Effect Context Sums
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Rather than using a method dictionary, in order to avoid lifting/unlifting
altogether, we use effect context sums. For example, consider the following
hypothetical method dictionary::

    data MonadDbGet m = MonadDbGet
      { dbGet :: DBTag -> ByteString -> m (Maybe ByteString)
      }

There are two implementations for it::

    dbGetRocks :: MonadRealDB ctx m => DBTag -> ByteString -> m (Maybe ByteString)
    dbGetRocks tag key = getDBByTag tag >>= rocksGetBytes key

    dbGetPure :: MonadPureDB ctx m => DBTag -> ByteString -> m (Maybe ByteString)
    dbGetPure (tagToLens -> l) key =
        view (l . at key) <$> (view (lensOf @DBPureVar) >>= readIORef)

In case of ``dbGetRocks``, the necessary context for this operation is having
``NodeDBs`` (implied by ``MonadRealDB``). In case of ``dbGetPure``, the
necessary context is having ``DBPureVar``.

Rather than put ``MonadDbGet`` effect dictionary into the ``ReaderT`` environment, we
construct a sum of all possible context values::

    data DBSum = RealDB NodeDBs | PureDB DBPureVar

This sum is used to implement a method that can use either one of the
implementations::

    eitherDB
        :: (MonadReader ctx m, HasLens DBSum ctx DBSum)
        => ReaderT NodeDBs m a -> ReaderT DBPureVar m a -> m a
    eitherDB ract pact = view (lensOf @DBSum) >>= \case
        RealDB dbs -> runReaderT ract dbs
        PureDB pdb -> runReaderT pact pdb

    dbGetSum
        :: MonadDBSum ctx m
        => DBTag -> ByteString -> m (Maybe ByteString)
    dbGetSum tag key =
        eitherDB (dbGetRocks tag key) (dbGetPure tag key)

There are two issues with this approach:

* it requires us to know up front about all of the available implementations,
  severely limiting extensibility

* when we need to assume a certain implementation, we have to fail at runtime,
  because the implementation is not reflected in types

ReaderT with Method Dictionaries
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Very similar to ``SendActions``, we have ``TxpGlobalSettings`` (see `file`__).
``TxpGlobalSettings`` is also a method dictionary, only difference is:

__ https://github.com/input-output-hk/cardano-sl/blob/8dcf8e947cfe9d70c454ad24029f064b022e1830/txp/Pos/Txp/Settings/Global.hs#L51

* ``SendActions`` is passed as explicit parameter to every function
* ``TxpGlobalSettings`` is passed as part of context put into ``ReaderT``

Problem Definition
------------------

Informed by previous failures, we are in a position to finally find a good
approach to monadic effects in our code. Ideally, with all of the pros and
none of the cons. So, to start, here's a checklist of properties we want:

* Flexibility. A flexible effect system allows to easily add an effect to a
  monadic stack locally, and to run effects partially and in arbitrary order.
  There also should be a way to have different implementations for the same
  effect.

* Extensibility. Adding a new effect must be cheap and modular. A thousand
  interconnected instances just won't cut it.

* Ease of use. We don't want to ``lift . lift . lift``.

* Compile-time performance. No type families, no instance search tricks. We've
  been bitten by this before. Keep it simple.

* Run-time performance. We want a flattened runtime representation for
  Reader-isomorphic effects.

* Run-time configurability. It's fine to keep track of effects themselves in the
  types, but the choice of an implementation must be delegated to terms. For
  instance, choosing between a real DB (RocksDB) and pure DB must be possible
  with a CLI option.

* Predictability. It must be easy manipulate effects in a predictable manner,
  without fearing that ``runReaderT`` will affect anything but ``MonadReader``.

Proposed Solutions
------------------

The ReaderT Pattern
~~~~~~~~~~~~~~~~~~~

Read the full article: https://www.fpcomplete.com/blog/2017/06/readert-design-pattern

The general sentiment of the article is to have a single ``Env`` data type
(basically, what we have with our modes), and use ``ReaderT Env IO`` exclusively.

The general sentiment is true: ``StateT``, ``WriterT``, and ``ExceptT`` are almost
never what you actually want. ``StateT`` kills the story for concurrency and makes
it harder to deal with exceptions, ``WriterT`` is simply broken performance-wise
(and its CPS-ed version is ``StateT`` with the same caveats), and ``ExceptT`` adds
one more way to throw exceptions (so you need one more way to catch them).

For pure code (without ``IO`` or ``MonadIO``), use ``StateT``, ``WriterT``, and
``ExceptT``. In other cases, avoid them.

This aspect of the ``ReaderT`` pattern makes a lot of sense. Monad transformers
are used to model effects in pure code, and in ``IO`` you don't have a choice but
to have the actual effects themselves (there's mutable state, there are
exceptions). Mixing them is unhelpful.

Now, let's consider the global ``Env`` type. The issue with it is associated
boilerplate of ``Has`` classes. In case we want to extend the environment to
introduce local effects, there's a high cost in boilerplate. This can be already
observed in our codebase.

Point-by-point rundown:

* Flexibility: LOW/MODERATE (depends on whether we have a newtype over
  ``ReaderT``). Extension of environment requires declaring a new type, writing
  ``Has`` instances for it, and potentially monad instances -- but there are
  ways to work around that (using overlappable tuple instances). Although the
  approach recommends avoiding monad classes in favor of ``Has`` classes, not
  everything is under our control and there are classes by external libraries.
  In case there's a newtype over ``ReaderT``, there's no way to partially run
  effects -- one needs to supply the entire ``ReaderT`` environment up front. In
  case there's no newtype over ``ReaderT``, we can extend the envirnoment using
  ``withReaderT``.

* Extensibility: MODERATE. Defining new effects boils down to declaring a method
  record and a ``Has`` class for it. Then, for each field in the method record we
  must define a function that extracts this method using the ``Has`` class and
  runs it. Boilerplate-heavy, yes, but at least effects don't need to know about
  each other.

* Ease of use: MODERATE. Beginners can quickly grasp the basic concepts, but
  there are pitfalls when it comes to lifting method records (see section on
  ``SendActions``).

* Compile-time performance: SUPERB. There's nothing to it, GHC can easily handle
  this style of code.

* Run-time performance: MODERATE. A flat ``ReaderT`` layer is good, but method
  records are stored within the context (rather than passed via instance
  search). Because of this, the compiler can't assume that the method record is
  coherent (it's not, you can modify it with ``local``), and it limits inlining.
  Instance specialization can't be performed because there are no instances to
  speak of.

* Run-time configurability: SUPERB. You can put just about anything into these
  manually defined method records, modify them at will, etc. However, we need to
  figure out a good story for method records that depend on other method
  records, because it's the same pitfall as with ``SendActions``: how can we
  ensure that if we change something (say, a logging method) with ``local``,
  other method records that use logging will be updated accordingly?

* Predictability: LOW/MODERATE (depends on whether we have a ``newtype`` over
  ``ReaderT``). In case there's no newtype, ``runReaderT`` can change the
  behavior, which is counter-intuitive. In case there's a newtype, there's too
  much freedom with regards to what can be put into method records.

Verdict: the approach is viable but has its costs.


Dictionary-Passing Style
~~~~~~~~~~~~~~~~~~~~~~~~

Similar to previous approach and one was widely used by the networking team.

As we understand ideas behind it:

- Single base monad

  - Similarly to ``ReaderT IO`` approach

- Differentiating implementations all stored in records

  - No type classes and instances like ``instance MyClass (ReaderT Ctx IO)``
  - Each function accepts a single **explicit** parameter of type ``Ctx m``
  - This type ``Ctx`` contains all needed methods

Point-by-point rundown:

* Flexibility: MODERATE. (Same as above).

* Extensibility: GOOD. (Same as above, minus a bit of boilerplate helpers).

* Ease of use: LOW. (Same as above, plus one needs to manually pass the
  context everywhere).

* Compile-time performance: SUPERB. (Same as above).

* Run-time performance: MODERATE. (Same as above).

* Run-time configurability: SUPERB. (Same as above)

* Predictability: MODERATE. Checking what implementation is used is easy as tracing
  the provenance of the input context, but there's too much freedom with regards to
  what can be put into the method records.

Compared to the ``ReaderT``-based approach, explicit dictionary passing trades
ease of use for simpler types (not a single monad transformer), less boilerplate at
effect definition site (no need for helpers that ``ask`` and run a method, as
it's done at use site), and better predictability.

Verdict: preferable to ``ReaderT`` as long as we can put up with reduced
convenience.

ReaderT over Abstract Base
~~~~~~~~~~~~~~~~~~~~~~~~~~

Previous two solutions are widely known and used.
One proposed here is more exotic, but has some benefits over previous two:

- Allows to have visibility which functions have full `IO` access

  - Which is not the case with first approach

- Doesn't require high transition costs from current codebase yet being general

  - In fact `Mockable` type classes already utilize very similar ideas

Motivation
____________________

After a discussion, @georgeee and @int-index came up with a hybrid
approach that combines many elements of other approaches to provide a
comprehensive solution.

Design goals (in addition to already discussed criteria):

* avoid data types parametrized by an abstract monad ``m``
  (including explicit method dictionaries),
  because it makes reasoning about code complicated

* avoid hard-coded ``IO``, ``MonadIO``, ``MonadBaseControl IO``, etc,
  in order to have good mocking capabilities and separation of concerns

And here are some observations:

* operationally, our monads can be ``ReaderT ctx IO`` for production code
  and ``ReaderT ctx (CatchT ST)`` (or similar) in pure tests. Both of these
  provide (1) passing context, (2) throwing/catching exceptions, (3) mutable
  state.

* runtime configuration of effects arises for CLI parameters or YAML configuration
  (for instance, the slotting method is affected by the '--no-ntp' flag). For this
  use cases effect context sums must be sufficient.

* presumably, any effect dictionary can be separated into its closure (context)
  and top-level operations

Proposed approach
_________________

With these facts established, let us consider the approach from multiple angles:
how effect definitions would look like, and how effect implementatinos look like.

First, let us have a few base monads::

    newtype Prod a = Prod (IO a)
    newtype TestIO a = TestIO (IO a)
    newtype TestPure a = TestPure (CatchT ST a)

To define an effect, in the most general case we will need two classes and a type family.
In the first class we define the methods, mtl-style. The second class is a ``Has``-class to
extract a part of a context. And a type family determines what context we need for the effect::

    class MonadFoo b where
        type FooContext b
        sepulate :: HasFooContext b ctx => ReaderT ctx b Sepulki
        patanize :: HasFooContext b ctx => ctx
                                        -> ReaderT ctx b a
                                        -> ReaderT ctx b (Either Patak a)

    class HasFooContext b ctx where
        fooContext :: ctx -> FooContext b

A few things to notice:

* the parameter ``b`` stands for "base monad", it will be instantiated to
  something simple like ``Prod`` or ``TestPure``.

* all methods are defined in the ``ReaderT ctx b`` monad.

* we are fine with this monad being used in a negative position (in
  ``patanize``), because it is not meant to be lifted/unlifted in any
  circumstances.

Usage examples
______________

This is how we could define database-related monads this way::

    class MonadDBGet b where
        type DBContext b
        dbGet :: HasDBContext b ctx => DBTag -> ByteString -> ReaderT ctx b (Maybe ByteString)

    class HasDBContext b ctx where
        dbContext :: ctx -> DBContext b
        -------- or: Lens' ctx (DBContext b)

The concrete implementations are defined separately from the class,
taking the context as an explicit parameter::

    --       Constraints necessary for the impl (can contain 'MonadIO' if the impl is prod-specific,
    --             \                             but preferably shouldn't)
    --              \
    --               \          NodeDBs are a RocksDB-specific context
    --                \                  /
    dbGetRocks :: MonadRealDB ctx m => NodeDBs -> DBTag -> ByteString -> ReaderT ctx m (Maybe ByteString)
    dbGetRocks = ...

    -- Constraints for the impl (no 'MonadIO', because it's used for tests)
    --           \
    --            \               DBPureVar is a pure-db specific context
    --             \                /
    dbGetPure :: MonadPureDB m => DBPureVar -> DBTag -> ByteString -> ReaderT ctx m (Maybe ByteString)
    dbGetPure = ...

We can use these implementations to define instances for the base monads::

    instance MonadDBGet Prod where
        type DBContext Prod = NodeDBs
        dbGet tag key = do
          nodeDBs <- asks dbContext
          dbGetRocks nodeDBs tag key

    instance MonadDBGet TestPure where
        type DBContext TestPure = DBPureVar
        dbGet tag key = do
          dbPureVar <- asks dbContext
          dbGetPure dbPureVar tag key

These instances are defined not in the modules with implementations, but in the modules
with the base monads (``Prod`` and ``TestPure``), similarly to what we have now.

The implementation-specific context is extracted in these method definitions.

In case of ``MonadDBGet``, the choice of implementation is determined by the
base monad (in prod we use rocksdb, in tests we use pure db). However, we also
have cases where we need runtime configurability, when with the same base monad
there can be different implementations. For instance, for slotting we can have
ntp-based slotting or simple slotting, depending on whether the ``--no-ntp`` flag
was passed. In these cases we choose to use effect sums::

    instance MonadSlotting Prod where
        type SlottingContext Prod = Either SimpleSlottingVar NtpSlottingVar
        getCurrentSlot = do
          sctx <- asks slottingContext
          either getCurrentSlotSimple getCurrentSlotNtp sctx

While effect context sums are closed, we are fine with it because we don't
need extensibility at instance definition site, while the implementations
themselves are defined independently.

Another example is passing ``SendActions``. With the new approach instead of
``SendActions`` we should be passing ``SendContext`` (that contains the IP of
the peer, etc), and define ``MonadSend`` to implement the actual sending logic.

Context extensibility
_____________________

In order to address the issue of extensibility, for each ``Has``-class we can
define two instances in a generic way::

    class HasFooContext b ctx where
        fooContext :: ctx -> FooContext b

    instance HasFooContext b (FooContext b, _) where
        fooContext = fst

    instance {-# OVERLAPPABLE #-} HasFooContext b ctx => HasFooContext b (_, ctx) where
        fooContext = fooContext . snd

This way it's possible to add a context using ``withReaderT (fooCtx,)``. A
stylistic issue with this is the use of ``{-# OVERLAPPABLE #-}``, but it seems
rather benign here. A technical issue is that need ``FooContext`` to be a data
family (rather than a type family) in order for this to work, but it's a simple
change.

For instance, consider ``NtpMonad``. Right now we pass ``NtpSlottingVar`` manually
there. Instead we could add a ``HasNtpSlottingVar ctx`` constraint, and when running
the methods write ``withReaderT (ntpSlottingVar,)``.

Usage examples from codebase
____________________________

Finally, now that we know how to define effects with these approach, let's see
how to use them. Let's take a look at a concrete example, ``usNormalize``::

    usNormalize :: (USLocalLogicMode ctx m) => m ()
    usNormalize = do
        tip <- DB.getTip
        stateVar <- mvState <$> views (lensOf @UpdateContext) ucMemState
        atomically . writeTVar stateVar =<< usNormalizeDo (Just tip) Nothing

The type signature will change a little, so that we have an explicit ``ReaderT``,
but an abstract base monad::

    usNormalize :: (USLocalLogicMode ctx b) => ReaderT ctx b ()
    usNormalize = ...

The implementation will change too, but only a little. We won't use ``MonadIO``
anymore, so instead of using ``atomically :: STM a -> IO a`` we will use our
version of it, ``atomically :: MonadSTM m => STM a -> m a``. Since ``STM`` requires
``IO`` in the end, it will be mocked only in ``TestIO``, not in ``TestPure``.

Another example is ``verifyBlocks``::

    type TxpGlobalVerifyMode m =
        ( TxpCommonMode m
        , MonadError ToilVerFailure m
        )

    verifyBlocks
        :: forall m. TxpGlobalVerifyMode m
        => Bool -> OldestFirst NE TxpBlock -> m (OldestFirst NE TxpUndo)

In this case it also requires ``MonadError``, which is later satisfied with
``ExceptT``. We don't allow ``ExceptT`` anymore (except in 100% pure code),
so it must be rewritten to use ``MonadThrow``. The reason is that if we have
multiple ways to throw, we need multiple ways to catch (and bracket). With the
new approach, the type of ``verifyBlocks`` will look like this::

    type TxpGlobalVerifyMode ctx m =
        ( TxpCommonMode ctx m
        , MonadThrow m
        )

    verifyBlocks
        :: forall m. TxpGlobalVerifyMode ctx m
        => Bool -> OldestFirst NE TxpBlock -> ReaderT ctx m (OldestFirst NE TxpUndo)


Transition Period
_________________

Rewriting the entire codebase to this new approach in one go would be an
unsurmountable task. Fortunately, it is compatible with what we have now,
and we can do it in chunks.

* the ``Mockable`` machinery that we use can be left almost intact, but
  the constraints will apply only to the base monad

* the ``reflection`` machinery can be left completely intact (furthermore,
  it's likely that we don't want to change it at all, as it works well)

* the ``MonadIO`` constraints can be removed over time, localizing the scope
  of their use.

To elaborate on the point about ``MonadIO``, right now we have constraint
synonyms like this::

    type MonadX m = (Mockable CurrentTime m, Mockable Async m, MonadIO m)

but we can change it to::

    type BaseX m = (Mockable CurrentTime m, Mockable Async m)
    type BaseXIO m = (BaseX m, MonadIO m)

Having two constraints provides better visibility for where we actually need the
``IO`` capabilities (and can launch missilies, which isn't nice), and we can
gradually remove these occurences from our codebase.

After the transition is complete, all ``MonadIO`` constraints will be replaced
with abstract ones, such as ``Mockable``::

    type BaseX m =
      ( Mockable CurrentTime m
      , Mockable Async m
      , Mockable XXX m
      , Mockable YYY m
      , MonadCath m )

In case of ``MonadCatch``, we use it rather than ``Mockable Catch`` to have
better compatibility with other libraries. In the grand scheme of things it's
not important whether we use ``Mockable``-based machinery or mtl-style classes
to abstract ``IO`` functionality.

Point-by-point rundown
______________________

Let us have a point-by-point rundown of the approach properties:

* Flexibility: GOOD. We can use an effect locally by extending the reader
  context, provided that the base monad supports it. We don't need to use monad
  transformers to add an effect. We can run effects partially (by supplying only
  a part of the context). We can have different prod/test base monads.

* Extensibility: MODERATE. To define an effect we need two classes and a type/data
  family, and the ``Has``-class presents a certain amount of boilerplate. Other than
  that, it is good: we don't have the ``N*M`` problem.

* Ease of use: GOOD (hopefully). We don't have lifting, hoisting, etc, just write
  the code in ``ReaderT`` over an abstract base monad.

* Compile-time performance: GOOD. While we do have type families, they only require
  a single step of reduction, so this is not an issue.

* Run-time performance: GOOD. Since we use classes, the methods should be
  specialized to concrete monads, and we pass the context in a flat ``ReaderT``.

* Run-time configurability: GOOD. We identify two categories of effect impls --
  those that depend on the base monad and those that don't. In the first case
  we don't need configurability, in the second we use effect context sums.

* Predictability: SUPERB. While the types do affect the choice of implementation,
  all code is written in a single monad, so it's easy to track which
  implementation is used where. (We forbid ``IdentityT`` redirection layers).
  It's extremely easy to find out what code is executed for a particular impl,
  because it's in a top-level instance, not in a method dictionary.

As you can see, the hybrid approach is a compromise. There are still issues,
but there are no fatal flaws (we don't score LOW for any of the criteria).
