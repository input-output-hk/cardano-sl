# cardano-sl-util

A library of utility data types are functions for Cardano SL, including:

* Functions for collecting compile time version information.
* Extra concurrency primitives, including more operations on `MVar`s, `RWLock`s
  lifted to `MonadIO` and a `PriorityLock`.
* File system related utility functions.
* A futures capability.
* Utility function for left justifying/aligning text.
* A logging capability used in the generator, explorer, wallet and elsewhere.
* Utility functions to extend the `LRU` functionality defined in the `lrucache`
  package.
* A `MapModifier` type that collects modifications (insertions and deletions) on
  a `Map` like type.
* A collection of orphan instances for external data types.
* A collection of QuickCheck helpers and Arbitrary instances.
* A `Some` data type that allows a constraint to turned into an existential type.
* A restart-able, STM-based timer.

## Logging using monad "WithLogger m": 

see examples in test/Test/Pos/Util/LogSpec.hs
and haddock for module `Pos.Util.Log`

## Logging using contravariant "Trace": 

see examples in test/Test/Pos/Util/TraceSpec.hs
and haddock for module `Pos.Util.Trace`

several approaches exist:

1.) `TraceIO` is a `Trace IO (Severity Text)`
    simple logging of severity and message to 

2.) `Trace m LogItem`
    LogItem is a triple of privacy, severity and the log message.
    Privacy indicates to which log files the message is writable: 
    public, private, or both.
    Severity is one of: Debug, Info, Notice, Warning, Error

3.) `TraceNamed` is a `Trace m (LogNamed LogItem)`
    it carries a stack of log context names, on which a new name
    is added with `appendName`, and a LogItem.

