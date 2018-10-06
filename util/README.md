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

## Logging

### Context naming

In all logging modes, it is handy to name the context in which logging happens.
This named context is added to the log message. The root is always named 'cardano-sl'.
The context naming is similar to a stack: entering a new named context 
corresponds to pushing the name onto the naming stack. On leaving the context,
the named context is reset to the previous version, thus dropping the last name.

in compatibility mode (Pos.Util.Wlog):
```
import Pos.Util.Wlog (WithLogger, logInfo, addLoggerName)

interestingContext :: WithLogger m => m ()
interestingContext = do
    addLoggerName "interestingContext" $ do
        logInfo "we now are in a new context"
        evalOtherContext 42
```
its output will look like this:
```
[cardano-sl.interestingContext:Info:ThreadId 123] [2018-10-04 06:30:23.01 UTC] we now are in a new context
```

to produce the same output in 'Trace' logging (Pos.Util.Trace.Named):
```
import Pos.Util.Trace.Named (TraceNamed, appendName, logInfo)

interestingContext :: (MonadIO m) => TraceNamed m -> m ()
interestingContext logTrace0 = do
    let logTrace = appendName "interestingContext" logTrace0
    logInfo logTrace "we now are in a new context"
    evalOtherContext logTrace 42
```

### Structured logging

Structured logging enables us to record JSON objects and directly work on them (e.g. using 'jq' queries).

In this example we output the time from one slot to the next in the field "data":
```
{"at":"2018-10-03T12:37:57.001766381Z","env":"bench:1.3.0","ns":["cardano-sl","node","slotting"],"data":{"TimeDiff":"19998265"},
"app":["cardano-sl"],"msg":"","pid":"13560","loc":null,"host":"hostname","sev":"Info","thread":"ThreadId 7532"}
```
or nicely formatted with jq:
```
{
  "at": "2018-10-03T12:37:57.001766381Z",
  "env": "bench:1.3.0",
  "ns": [
    "cardano-sl",
    "node",
    "slotting"
  ],
  "data": {
    "TimeDiff": "19998265"
  },
  "app": [
    "cardano-sl"
  ],
  "msg": "",
  "pid": "13560",
  "sev": "Info",
  "thread": "ThreadId 7532"
}
```

Only JSON `Object`s (key->value maps) can be logged through the structured logging functions
`logDebugX` through `logWarningX`. Other variants of JSON values will silently be ignored.

For the above structured output of a `TimeDiff`, the following instance was implemented:
(in `core/src/Pos/Core/Slotting/TimeDiff.hs`)
```
instance ToObject TimeDiff where
    toObject (TimeDiff usec) = singleton "TimeDiff" $ String $ show $ toMicroseconds usec
```

Another way of outputting structures as `Object` is via `ToJSON`. The class `ToObject` (in `Pos.Util.Log`)
has a default implementation of `toObject` which accepts `ToJSON` values:
```
import Pos.Util.Log (ToObject (..))
import Pos.Util.Log.Structured (logInfoX)
import Pos.Util.Wlog (WithLogger, logInfo, addLoggerName)

-- | a loggable data structure with JSON representation
data Loggable = Loggable {
                  _fieldN :: Int
                , _fieldS :: String
                }
                deriving (Show, Generic)
instance ToJSON Loggable
instance ToObject Loggable


evalOtherContext n = do
    logInfoX (Loggable n "msg")

interestingContext :: WithLogger m => m ()
interestingContext = do
    addLoggerName (<> "interestingContext") $ do
        logInfo "we now are in a new context"
        evalOtherContext 42
```


### Output selection

Logging is setup via a `LoggerConfig` and includes:
* log rotation parameters
* compatiblity to old format with shortcuts like 'files:' etc.
* handlers are instances of output backends
  * FileTextBE - textual output to files
  * FileJsonBE - outputs JSON representation to files
  * StdoutBE, StderrBE - output to `/dev/stdout` or `/dev/stderr`
  * DevNullBE - no output at all

As an example, this is the configuration for `Daedalus`:
```
# This config is used by Daedalus (in production).

rotation:
    logLimit: 5242880 # 5MB
    keepFiles: 10

loggerTree:
  severity: Info+
  cardano-sl.syncWalletWorker: Error+
  files:
    - node

  handlers:
    - { name: "JSON"
      , filepath: "pub/node.json"
      , logsafety: PublicLogLevel
      , severity: Info
      , backend: FileJsonBE }

```

- it sets up log rotation to keep the ten most recent files, where each
has size of at most five megabytes.
- under "loggerTree", the compatiblity to the old format, it defines
global severity filter to be at least 'Info', and for the named context 
"cardano-sl.syncWalletWorker" only errors are output. The keyword 'files:' 
lists the output files which are created.
- under 'handlers:' a list of outputs can be defined. Here we open a JSON 
backend to a file and only output messages with severity at least 'Info' and 
marked "public" (`LogSecurityLevel` is one of: `SecretLogLevel`, `PublicLogLevel`;
log files marked "secret" contain all messages, whereas "public" log files 
contain masked log messages where identifying information is hidden)

