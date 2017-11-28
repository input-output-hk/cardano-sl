# How to Build Cardano SL and Daedalus from Source Code

This manual describes how to build Cardano SL and Daedalus from the source code.

## Cardano SL and Daedalus Bridge

Cardano SL consists of a collection of binaries that constitute
the backend, a PureScript API for the Electron-based wallet, and the
Electron-based wallet called “Daedalus”.

The source code for both Cardano SL and Daedalus Bridge can be obtained
from the [official repository](https://github.com/input-output-hk/cardano-sl).

Cardano SL supports two ways for building itself:

-   (preferred) [Nix](https://nixos.org/nix/) package manager (backed by a binary cache by IOHK continuous integration)
-   [Stack](https://haskellstack.org) with Nix for system libraries

In any case, we strongly suggest using [Nix package manager](https://nixos.org/nix/download.html) to get the correct dependencies for building Cardano SL. It will fetch the correct `openssl` version, but won't override the system-installed version. The following commands assume that you already has `stack` and `nix-*` programs.

### Binaries

As a result of building Cardano SL, you will get a set of components (binary files). This set includes the main node for Cardano SL network and various helper tools. Please read [this page of the documentation](https://cardanodocs.com/technical/cli-options/) for technical details.

## Common build steps

The following steps are shared between the two methods of building Cardano: fetching source and deciding on a branch to be built.

Clone Cardano SL repository and go to the root directory:

    $ git clone https://github.com/input-output-hk/cardano-sl.git
    $ cd cardano-sl

Switch to the latest release branch, for example, `cardano-sl-1.0`:

    $ git checkout cardano-sl-1.0

## Nix build mode (recommended)

First, prerequisite: install Nix (full instructions at https://nixos.org/nix/download.html):

    curl https://nixos.org/nix/install | sh

Two steps remain, then:

1.  To employ the signed IOHK binary cache:

        $ sudo mkdir -p /etc/nix
        $ sudo vi /etc/nix/nix.conf       # ..or any other editor, if you prefer

    ..and then add two following lines:

        $ binary-caches             = https://cache.nixos.org https://hydra.iohk.io
        $ binary-caches-public-keys = hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=

2.  Actually building the Cardano SL node (or, most likely, simply obtaining it
    from the IOHK's binary caches) can be performed by building the attribute `cardano-sl-static`:

        $ nix-build -A cardano-sl-static --cores 0 --max-jobs 2 --no-build-output --out-link cardano-sl-1.0

    The build output directory will be symlinked as `cardano-sl-1.0` (as specified by the command), and it will contain:

        $ ls cardano-sl-1.0/bin
        cardano-node-simple

NOTE: the various other Cardano components can be obtained through other attributes:

-  `cardano-report-server-static`:
   - `cardano-report-server`
-  `cardano-sl-auxx`:
   - `cardano-auxx`
-  `cardano-sl-explorer-static`:
   - `cardano-explorer`, `cardano-explorer-hs2purs`, `cardano-explorer-swagger`, `cardano-explorer-mock`
-  `cardano-sl-tools-static`:
   - `cardano-analyzer`, `cardano-dht-keygen`, `cardano-genupdate`, `cardano-keygen`, `cardano-launcher`, `cardano-addr-convert`, `cardano-cli-docs`, `cardano-block-gen`, `cardano-post-mortem`
-  `cardano-sl-wallet`:
   - `cardano-node`, `cardano-wallet-hs2purs`, `cardano-swagger`

In general, for any given cabal `PACKAGE` provided by Cardano, there is a
corresponding Nix attribute for it -- `PACKAGE`, and sometimes, in case of
packages providing executables, the `PACKAGE-static` also provides a
statically-linked variation.

## Stack with Nix for system libraries (mixed mode)

Please, see the previous section on how to enable use of the IOHK binary cache.

Enter `nix-shell`:

    $ nix-shell

After that, in order to build Cardano SL with wallet capabilities, run the following script:

    [nix-shell:~/cardano-sl]$ ./scripts/build/cardano-sl.sh

It is suggested having at least 8GB of RAM and some swap space for the build process. As the project is fairly large and GHC parallelizes builds very effectively, memory and CPU consumption during the build process is high. Please make sure you have enough free disk space as well.

After the project is built - it can take quite a long time -  the built binaries can be launched using the `stack exec` command. Let's discuss important binaries briefly before proceeding to the next step.

## Daedalus Wallet

Let's proceed with building the wallet. First of all, let's build Daedalus Bridge.

### Building Daedalus Bridge

Currently Nix expressions don't install Node.js and NPM, so those have to be installed manually.
To do that, consult the repositories of the package manager of the according OS, or [download binaries](https://nodejs.org/en/download/). Please make sure you have Node.js version 6. You can use [nvm](https://github.com/creationix/nvm#installation)
to install proper version.

Now run the following script:

    [nix-shell:~/cardano-sl]$ ./scripts/build/daedalus-bridge.sh

After that `daedalus-client-api` will be registered in the local NPM package repository. This way, at any time, `daedalus-client-api` dependency can be satisfied in any project that depends on it by manually running following command:

    [nix-shell:~/cardano-sl]$ npm link daedalus-client-api

### Building Daedalus

Clone Daedalus repository and go to the root directory:

    [nix-shell:~/cardano-sl]$ cd
    [nix-shell:~/cardano-sl]$ git clone https://github.com/input-output-hk/daedalus.git
    [nix-shell:~/cardano-sl]$ cd daedalus

Then run the following script:

    [nix-shell:~/daedalus]$ ./scripts/link-bridge.sh
    [nix-shell:~/daedalus]$ npm install

### Running acceptance tests

To run acceptance tests one first has to have cluster running. We can run cluster on our machine with:

    [nix-shell:~/cardano-sl]$ ./scripts/launch/demo-with-wallet-api.sh

Then navigate to daedalus repo and run tests server with:

    [nix-shell:~/daedalus]$ npm run hot-server

and in the seperate terminal window run tests:

    [nix-shell:~/daedalus]$ npm run test

You should see acceptance tests being run for about 5 minutes. Note that acceptance tests will be actively be taking window focus until they are finished. If it complains about `cardano-node.log` not existing just create it in the path with:

    [nix-shell:~/daedalus]$ touch ~/.config/Daedalus/Logs/cardano-node.log
