# Testing update system locally

## Prerequisites

Additional utilities to be installed:

* webfs (webfsd) server
* uxterm to emulate UI when no `$UI` provided

## With Daedalus

```
UI=Daedalus.sh ./scripts/launch/us-test.sh
```

This will launch nodes, Daedalus

Print `start` in original console to propose an update.

## Without Daedalus

```
./scripts/launch/us-test.sh
```

This will launch nodes, ui emulation as simple `uxterm` terminal with incitation to write `update` to emulate UI closing with exit code 20.

Print `start` in original console to propose an update.

