# cardano-sl-util

A library of utility data types are functions for Cardano SL, including:

* Functions for collecting compile time version information.
* Extra concurrency primitives, including more operations on `MVar`s, `RWLock`s
  lifted to `MonadIO` and a `PriorityLock`.
* File system related utility functions.
* A futures capability.
* Utility function for left justifying/aligning text.
* A `LoggerName` capability used in the generator, explorer, wallet and elsewhere.
* Utility functions to extend the `LRU` functionality defined in the `lrucache`
  package.
* A `MapModifier` type that collects modifications (insertions and deletions) on
  a `Map` like type.
* A collection of orphan instances for external data types.
* A collection of QuickCheck helpers and Arbitrary instances.
* A `Some` data type that allows a constraint to turned into an existential type.
* A restart-able, STM-based timer.
