# Building with support for profiling

## stack build

Although stack has support for building with profiling [1], for some reason
it is currently unable to build `cardano-sl-update` with profiling enabled.

```
stack build --profile cardano-sl-wallet-new:wallet-unit-tests
```

will result in

```
    ByteCodeLink.lookupCE
    During interactive linking, GHCi couldn't find the following symbol:
      textzm1zi2zi2zi2zm9UQZZjEJZZQFSGMffj1ZZ5g00_DataziText_zdfMonoidTextzuzdczlzg_closure
    This may be due to you not asking GHCi to load extra object files,
    archives or DLLs needed by your current session.  Restart GHCi, specifying
    the missing library using the -L/path/to/object/dir and -lmissinglibname
    flags, or simply by naming the relevant files on the GHCi command line.
    Alternatively, this link failure might indicate a bug in GHCi.
    If you suspect the latter, please send a bug report to:
      glasgow-haskell-bugs@haskell.org
```

This seems to be related to a [ghc bug][2], but I can't figure out exactly
why this is going wrong because both nix and cabal are able to build it just
fine. The ghc ticket isn't very clear about this either. I even patched stack
to not pass `-fprof-auto` and `-fprof-cafs`, but it didn't make a difference,
nor did forcing stack to use my system ghc.

## `nix-build`

`nix-build` can be used to obtain a profiling build

```
nix-build -A cardano-sl-wallet-new --arg enableProfiling true
```

Note however that this _builds and runs_ the tests for a lot of  dependencies.
To address this, may wish to change the definition of `mkDerivation` in
`default.nix` to

```
mkDerivation = args: super.mkDerivation (args // {
    enableLibraryProfiling    = enableProfiling;
    enableExecutableProfiling = enableProfiling;
    doCheck                   = args.pname == "cardano-sl-wallet-new";
  }
```

to only build and run the tests for the specified package (it does not seem
to be possible to build but not run them).

TODO: The above should work but doesn't. Not totally sure why. Need to ask
somebody familiar with the `nix` infrastructure.

## `cabal new-build`

`cabal new-build` also has built-in support for profiling [3]. Provided that
a `cabal.project` has been created, we can specify profiling options like this:

```
package wallet-new
  profiling-detail: toplevel-functions
```

It also seems to be necessary to update to `unix-time-0.3.8` rather than
`unix-time-0.3.7` as the latter results in a compile time failure about
missing `config.h`.

Then:

```
cabal new-build --enable-profiling wallet-unit-tests
```

# Running

Depending on the build method, the executable will have been left in a
different location. To run the executable with profiling enabled, run

```
path/to/executable +RTS -p -RTS
```

this will leave a `.prof` file in the current directory with the profiling
results. A useful package for visualizing a timing profile is [Profiteur][4].

To obtain a heap profile, use one of the `-h` RTS options, for example

```
path/to/executable +RTS -hy -RTS
```

for a heap profile by type. This will leave a `.hp` file in the current
directory. This can be converted to a `.ps` file using `hp2ps`
(which comes bundled with ghc):

```
hp2ps -c wallet-unit-tests.hp
```

or to an `.svg` file using [`hp2pretty`][5]

```
hp2pretty wallet-unit-tests.hp
```

For more RTS options, run

```
path/to/executable +RTS -? -RTS
```

# References

[1]: https://docs.haskellstack.org/en/stable/GUIDE/#debugging
[2]: https://ghc.haskell.org/trac/ghc/ticket/14931
[3]: https://www.haskell.org/cabal/users-guide/nix-local-build.html#how-can-i-profile-my-library-application
[4]: https://hackage.haskell.org/package/profiteur
[5]: https://hackage.haskell.org/package/hp2pretty
