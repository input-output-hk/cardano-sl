# Testing update system locally

This instruction covers how to test software update locally:

  1. Prepare and propose software update
      a. Prepare alt configuration with changed application version constant
      b. Propose updated with script that simply replaces one configuration file (in setup) with other
  2. Launch wallet node with launcher, Daedalus (or script that serves as Daedalus emulator)
      a. When update arrives to Deadalus to finish, launcher will restart Daedalus + node

## Prerequisites

Additional utilities to be installed:

* webfs (webfsd) server
* uxterm to emulate UI when no `$UI` provided

## With Daedalus

Write command to launch Daedalus on your system to `daedalus-example.sh`

```
UI=Daedalus.sh ./scripts/launch/us-test.sh
```

This will launch nodes, Daedalus

Print `start` in original console to propose an update.

## Without Daedalus

```
./scripts/launch/us-test.sh
```

This will launch nodes, ui emulation as simple `uxterm` terminal with invitation to write `update` to emulate UI closing with exit code 20.

Print `start` in original console to propose an update.

