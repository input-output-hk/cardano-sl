# Using Nix with Cardano SL

We use Nix for building and deploying Cardano SL. It provides fully
deterministic and cached builds of our software and all dependencies,
and permits fully declarative configuration of the infrastructure
running the Cardano SL network.

## Installing

The [Nix](https://nixos.org/nix/) package manager can be installed on
any Linux distribution (with SELinux disabled) or on macOS.

Multi user mode is the best and most commonly tested configuration. To
install Nix in multi user mode:

    sh <(curl https://nixos.org/nix/install) --daemon

Full instructions are in the
[Nix Manual](https://nixos.org/nix/manual/#ch-installing-binary).

## Binary cache

Configuring the IOHK binary cache is essential for developing with
Cardano SL. If you do not have the binary cache set up correctly, Nix
will download source tarballs and proceed to build an entire system
from scratch.

Add the following lines to `/etc/nix/nix.conf`. If the file does not
exist, then create it.

    substituters         = https://hydra.iohk.io https://cache.nixos.org
    trusted-substituters =
    trusted-public-keys  = hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=

The `nix-daemon` must be restarted after editing `/etc/nix/nix.conf`
for changes to take effect. Run `systemctl restart nix-daemon` on Linux
or `sudo launchctl stop org.nixos.nix-daemon; sudo launchctl start
org.nixos.nix-daemon` on macOS.

We do not recommend using `~/.config/nix/nix.conf` unless you are a
power user. It's simpler to have only one global config file.

If using NixOS, see [`iohk-binary-cache.nix`](../nix/iohk-binary-cache.nix).

## Using command-line options

Rather than configuring the binary caches, they can be given as options to
`nix-build`, like so:

```sh
$ nix-build \
  --option substituters "https://hydra.iohk.io https://cache.nixos.org" \
  --option trusted-substituters "" \
  --option trusted-public-keys "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=" \
  default.nix
```

The user must be a "trusted" nix user. This can be set in [nix.conf](https://nixos.org/nix/manual/#name-11).
For NixOS systems, it can be set by the `nix.trustedUsers` option in
`/etc/nixos/configuration.nix`.

## Other configuration

You may wish to adjust the `max-jobs` and `cores` settings in
`nix.conf` for your computer.

    max-jobs = 2  # run at most two builds at once
    cores = 0     # the builder will use all available CPU cores

See the [Nix Manual](https://nixos.org/nix/manual/#ch-files) for more
information.


### Background Info

These papers have a good explanation of the problems solved by Nix and
are worth a read if you are interested.

 * [NixOS Paper](https://nixos.org/~eelco/pubs/nixos-jfp-final.pdf)

 * Eelco Dolstra's PhD Thesis,
   [_The Purely Functional Software Deployment Model_](https://nixos.org/~eelco/pubs/phd-thesis.pdf)


## FAQ

### I'm not getting cached builds!

If you have the develop/release/master branch checked out, and no
local changes, all builds should be downloaded from the IOHK binary
cache and nothing built locally.

1. Check that the IOHK binary cache is set up correctly.

   You can tell if the cache works by finding an output store path on
   [a Hydra build](https://hydra.iohk.io/job/serokell/cardano-sl/cardano-sl.x86_64-linux/latest#tabs-details),
   and then running `nix-store --realize PATH`. For example:

   ```
   nix-store --realize /nix/store/ivapcjym0ar5mdx3jyf1p6d3m2zzmajn-cardano-sl-1.3.0
   ```

   If this doesn't work, review your settings with:

   ```
   nix show-config | egrep '(subst|keys)'
   ```

2. Check for extra files in package sub-directories. Run `git clean
   -nd` to see what to delete.

   Common things which defeat the cache are things like benchmark
   results or files left behind after running tests.

   Run `git clean -ndX` to see what else to delete.

### Multi User Nix Installation

You can tell if Nix is installed in multi user mode by checking the
ownership of `/nix/store`. The `owner:group` should be `root:nixbld`.

Additionally, the `nix-daemon` service will be running in a multi user
Nix installation.


### How to update the `src.json` files

We pin versions to exact revisions and put these revisions, and their
SHA-256 hash into little json files. To recalculate the hash, use
`nix-prefetch-git` according to
[this procedure](https://github.com/input-output-hk/internal-documentation/wiki/Daedalus#q-how-to-change-cardano-sl-version-for-daedalus).

### Building `release.nix`

When building attributes from `release.nix`, don't forget to specify
the arch like this:

    nix-build release.nix -A cardano-wallet.x86_64-linux

If you don't specify the arch, it will build for both `x86_64-darwin`
and `x86_64-linux`. Unless you have a macos build slave configured,
this will fail with:

    checking for references to /tmp/nix-build-cardano-sl-util-1.2.0.drv-0 in /nix/store/8ncrzv05fbkb95l1n2l0mkiv94i23qb0-cardano-sl-util-1.2.0...
    error: a 'x86_64-darwin' is required to build '/nix/store/ncma07cbrlh344s28jrmk3r1n9c3rxxx-remove-references-to.drv', but I am a 'x86_64-linux'

### stack2nix Hackage Snapshots

If a non-LTS package version is added to `extra-deps` in `stack.yaml`
and then `stack2nix` fails with something like:

    user error (No such package formatting-6.3.6 in the cabal database. Did you run cabal update?)

Then it probably means that the Hackage snapshot is too old. Update
[`pkgs/generate.sh`](https://github.com/input-output-hk/cardano-sl/blob/develop/pkgs/generate.sh).

### Upgrading to Nix 2

In Nix 2 there is a command `nix upgrade-nix` to upgrade. However, no
such command exists before Nix 2.

The best way to upgrade from Nix 1.x is by uninstalling Nix and then
re-installing.

#### Finding your nix version

If you have Nix 2 then the `nix --version` command will
work. Otherwise, run `nix-build --version`.

To see what [channels](https://nixos.org/nix/manual/#sec-channels) you
have configured in your profile, run:

    nix-channel --list

#### Uninstalling nix on macOS

Run these commands in a terminal as the `root` user (`sudo -i`).

Remove the nix store and config files:

    rm -rf /nix
    rm -rf /var/root/.nix-*
    rm -rf /var/root/.cache/nix
    rm -rf /etc/nix

If the `/etc/*.backup-before-nix` files exist, move them back.

    mv /etc/profile.backup-before-nix /etc/profile
    mv /etc/bashrc.backup-before-nix /etc/bashrc
    mv /etc/zshrc.backup-before-nix /etc/zshrc

Look in `~/.bash_profile` and remove the lines which refer to nix.

Remove user-specific config files:

    rm -rf /Users/USER_NAME/.nix-*

Now close your terminal and open a new one to clear all the
environment variables.

#### Installing nix

Install nix again using the [instructions above](#installing).

A nix channel will be added for the latest stable nixpkgs release (see
`nix-channel --list`).


### How to build directly from GitHub

To build any branch straight from a GitHub tarball:

    nix-build https://github.com/input-output-hk/cardano-sl/archive/develop.tar.gz -A cardano-wallet


## Links

[NixOS](https://nixos.org/) is a complete Linux distribution based on
the Nix language.

The complete source of NixOS and the Nix packages collection are on
[GitHub](https://github.com/NixOS/nixpkgs).
