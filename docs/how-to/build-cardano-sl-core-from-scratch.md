## How to build Cardano SL Core from Scratch

This manual is tested on the clean Ubuntu 14.04.5 (64bit) installation. However it should work on other Linux distros and macOS as well.

## Get Cardano SL

Clone repository if you didn't do it yet:

```
$ git clone git@github.com:input-output-hk/cardano-sl.git
```

## Build Cardano SL Core

Now you have to prepare for building. You should do these preparing steps **only once**.

### Preparing: Nix

You have to install [`nix`](https://nixos.org/nix/) program:

```
$ curl https://nixos.org/nix/install | sh
$ . /home/USER/.nix-profile/etc/profile.d/nix.sh
```

where `USER` is your username.

### Preparing: Environment and Compiler

Now you have to setup build environment and install Haskell compiler:

```
$ cd cardano-sl
$ nix-shell
$ stack setup
```

Please note that these commands will take some time (probably 15 minutes or more).

After that open `stack`'s configuration file `~/.stack/config.yaml` and add these two lines at the end of this file:

```
nix:
  enable: true
```

Save file and close it.

Then do this command:

```
$ export TMPDIR=/tmp
```

### Building

Now you can build Cardano SL:

```
$ ./scripts/build/cardano-sl.sh
```

Please note that for the first time building will take some time (probably 25 minutes or more).
