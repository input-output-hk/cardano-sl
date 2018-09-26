# How to Build Cardano SL and Daedalus from Source Code

This manual describes how to build Cardano SL and Daedalus from the source code.

## Cardano SL and Daedalus

Cardano SL consists of a collection of binaries that constitute
the backend and the Electron-based wallet called “Daedalus”.

The source code of Cardano SL can be obtained from the
[official repository](https://github.com/input-output-hk/cardano-sl).

Cardano SL supports two ways for building itself:

-   (preferred) [Nix](https://nixos.org/nix/) package manager (backed by a binary cache by IOHK continuous integration)
-   [Stack](https://haskellstack.org) with Nix for system libraries

In any case, we strongly suggest using [Nix package manager](https://nixos.org/nix/download.html)
to get the correct dependencies for building Cardano SL. It will fetch the correct `openssl` version,
but won't override the system-installed version. The following commands assume that you already have
`stack` and `nix-*` programs.

### Binaries

As a result of building Cardano SL, you will get a set of components (binary files). This set includes
the main node for Cardano SL network and various helper tools. Please read
[this page of the documentation](https://cardanodocs.com/technical/cli-options/) for technical details.

## Common build steps

The following steps are shared between the two methods of building Cardano: fetching source and deciding
on a branch to be built.

Clone Cardano SL repository and go to the root directory:

    $ git clone https://github.com/input-output-hk/cardano-sl.git
    $ cd cardano-sl

Switch to the `master` branch:

    $ git checkout master

## Nix build mode (recommended)

First, prerequisite: install Nix (full instructions at https://nixos.org/nix/download.html):

    curl https://nixos.org/nix/install | sh

Two steps remain, then:

1.  To employ the signed IOHK binary cache:

        $ sudo mkdir -p /etc/nix
        $ sudo vi /etc/nix/nix.conf       # ..or any other editor, if you prefer

    and then add the following lines:

        substituters = https://hydra.iohk.io https://cache.nixos.org/
        trusted-substituters =
        trusted-public-keys = hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=

2.  Actually building the Cardano SL node (or, most likely, simply obtaining it
    from the IOHK's binary caches) can be performed by building the attribute `cardano-sl-node-static`:

        $ nix-build -A cardano-sl-node-static --cores 0 --max-jobs 2 --no-build-output --out-link master

    The build output directory will be symlinked as `master` (as specified by the command), and it will contain:

        $ ls master/bin
        cardano-node-simple

NOTE: the various other Cardano components can be obtained through other attributes:

-  `cardano-report-server-static`:
   - `cardano-report-server`
-  `cardano-sl-auxx`:
   - `cardano-auxx`
-  `cardano-sl-explorer-static`:
   - `cardano-explorer`, `cardano-explorer-swagger`, `cardano-explorer-mock`
-  `cardano-sl-tools`:
   - `cardano-analyzer`, `cardano-dht-keygen`, `cardano-genupdate`, `cardano-keygen`, `cardano-launcher`, `cardano-addr-convert`, `cardano-cli-docs`, `cardano-block-gen`, `cardano-post-mortem`
-  `cardano-sl-wallet-static`:
   - `cardano-node`, `cardano-swagger`

In general, for any given cabal `PACKAGE` provided by Cardano, there is a
corresponding Nix attribute for it -- `PACKAGE`, and sometimes, in case of
packages providing executables, the `PACKAGE-static` also provides a
statically-linked variation.

### Developing across multiple packages

A merge tool `cabal-merger` takes multiple cabal files and generates a single
top-level `everything.cabal` file that can be used for fast development across
the entire project with nix. When a cabal file is updated, running
`nix-shell -A updateEverythingCabal` will re-merge all the cabal files into
`everything.cabal` and `everything.nix`.

To develop across all packages in cardano-sl, run
`nix-shell default.nix -A everything.env`. This will run a nix-shell with all of
cardano-sl in scope. Then the following step is used to configure cabal:

    runhaskell Setup.hs configure --enable-tests

A repl can be opened with:

    runhaskell Setup.hs repl everything

And `ghcid` can monitor code for type errors as you edit it using:

    ghcid -c "runhaskell Setup.hs repl everything"

Note that repl can also target any executable or test.

### Running Tests during development

All tests can be ran during development using:

    runhaskell Setup.hs build
    runhaskell Setup.hs test --show-details=always

A single test package can be ran by specifying it as a parameter:

    runhaskell Setup.hs test --show-details=always cardano-test

To narrow down further, the repl can be used

    runhaskell Setup.hs repl cardano-test
    :main --match Test.Pos.Block.Cbor

### Developing a single package

A single package can be debugged in the same way to speed up building
using the following commands for example for `wallet-new`:

    nix-shell default.nix -A cardano-sl-wallet-new.env
    cd wallet-new
    runhaskell Setup.hs configure --enable-tests
    ghcid -c "runhaskell Setup.hs repl wallet-new"

Note: ghcid isn't included in the shell. It needs to be in your PATH.
This can be done by running `nix-env -f default.nix -iA ghcid`

### Profiling

Optionally, profiling can be enabled to track down performance issues using a couple more flags. This will take hours to build as profiled builds are
not cached by hydra. For example, to profile wallet unit tests:

    nix-shell -A everything.env default.nix --arg enableProfiling true
    runhaskell Setup.hs configure --enable-tests --enable-library-profiling --enable-profiling
    runhaskell Setup.hs build -j10 wallet-unit-tests
    runhaskell Setup.hs test wallet-unit-tests -- +RTS -p

## Stack with Nix for system libraries (mixed mode)

Please, see the previous section on how to enable use of the IOHK binary cache.

Enter `nix-shell`:

    $ nix-shell

If this is the first time you are initializing the nix-shell, install `ghc` first by running:

    [nix-shell:~/cardano-sl]$ stack setup

After that, in order to build Cardano SL with wallet capabilities, run the following script:

    [nix-shell:~/cardano-sl]$ ./scripts/build/cardano-sl.sh

Dependency version collisions have been encountered on macOS. If you run into something
[like this](https://github.com/input-output-hk/cardano-sl/issues/2230#issuecomment-354881696),
try running the following command from outside of a `nix-shell`

    $ nix-shell -p moreutils expect --run "unbuffer ./scripts/build/cardano-sl.sh | ts"

It is suggested having at least 8GB of RAM and some swap space for the build process. As the project
is fairly large and GHC parallelizes builds very effectively, memory and CPU consumption during the
build process is high. Please make sure you have enough free disk space as well.

After the project is built - it can take quite a long time -  the built binaries can be launched using
the `stack exec` command. Let's discuss important binaries briefly before proceeding to the next step.

## Stack build mode (for developers)

It is possible to build Cardano node with Stack only, without Nix.
Please note that in this case you have to install external dependencies
by yourself (see below).

### Install Stack

[Stack](https://docs.haskellstack.org/en/stable/README/) is a cross-platform program
for developing Haskell projects.

Recommended way, for all Unix-systems:

    $ curl -ssl https://get.haskellstack.org/ | sh

On macOS it is possible to install it with `brew`:

    $ brew install haskell-stack

### Setup Environment and Dependencies

To install Haskell compiler of required version run:

    $ stack setup

Then install C-preprocessor for Haskell:

    $ stack install cpphs

Finally install C-library for RocksDB.

On Ubuntu:

    $ sudo apt-get install librocksdb-dev

On macOS:

    $ brew install rocksdb

### Jemalloc Notice

Please make sure that you have [jemalloc](http://jemalloc.net/) package, version `4.5.0`.
If you have newer version of it - you will probably get linker errors during building.

### Building

Run the building script:

    $ cd cardano-sl
    [~/cardano-sl]$ ./scripts/build/cardano-sl.sh

## `cabal new-build` and Nix (experimental, for developers)

This type of build gives you a multi-package project with incremental
builds, where the all of the build dependencies (Haskell packages and
system libraries) are downloaded from the binary cache and available
in the shell.

See the previous section on how to set up the IOHK binary cache.

Before you start, install a recent version of `cabal` into your
profile. The package set used by cardano-sl (nixpkgs 18.03) only has
cabal 2.0.0.1 which doesn't work.

    [nix-shell:~]$ nix-env -f channel:nixos-18.09 -iA pkgs.cabal-install

Enter the `nix-shell`:

    [nix-shell:~/cardano-sl]$ nix-shell

Let cabal find its dependencies (already provided by the `nix-shell`):

    [nix-shell:~/cardano-sl]$ cabal new-configure

After that, to build all cardano-sl packages:

    [nix-shell:~/cardano-sl]$ cabal new-build all

To start a GHCi session for a component (wallet-new for example), run:

    [nix-shell:~/cardano-sl]$ cabal new-repl cardano-sl-wallet-new

### Known issues

 - `cabal-install` is not provided by the Nix shell environment. You
   have to install it yourself.
 - There needs to be a package version override in `cabal.project` for
   some unknown reason.


## Daedalus Wallet

Let's proceed with building the wallet.

### Building Daedalus

Clone Daedalus repository and go to the root directory:

    [nix-shell:~/cardano-sl]$ cd
    [nix-shell:~]$ git clone https://github.com/input-output-hk/daedalus.git
    [nix-shell:~]$ cd daedalus
    [nix-shell:~/daedalus]$ npm install

### Running acceptance tests

To run acceptance tests one first has to have cluster running. We can run cluster on our machine with:

    $ tmux
    [nix-shell:~/cardano-sl]$ ./scripts/launch/demo-with-wallet-api.sh

**Important**: you have to build a node with Stack (using `./scripts/build/cardano-sl.sh`) to run this
script.

Then navigate to daedalus repo and run tests server with:

    [nix-shell:~/daedalus]$ npm run hot-server

and in the seperate terminal window run tests:

    [nix-shell:~/daedalus]$ npm run test

You should see acceptance tests being run for about 5 minutes. Note that acceptance tests will be actively
be taking window focus until they are finished. If it complains about `cardano-node.log` not existing just
create it in the path with:

    [nix-shell:~/daedalus]$ touch ~/.config/Daedalus/Logs/cardano-node.log
