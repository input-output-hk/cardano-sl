# Contributors Guide

## Discussion

Developer discussion can be found on the official Telegram channel [here](https://t.me/CardanoDevelopersOfficial).
This channel is intended as a developer/contribution channel. It should not be used for customer support requests,
comedy/memes, or price talk. Feel free to discuss current issues, future possibilities, and visions for how you
think Cardano should progress.

## Bug Reports

Please [open an issue](https://github.com/input-output-hk/cardano-sl/issues/new)
to report about found bugs in Cardano SL.

The more detailed your report, the faster it can be resolved and will ensure it
is resolved in the right way.

## Code

If you would like to contribute code to fix a bug, add a new feature, or
otherwise improve Cardano SL, pull requests are most welcome. It is a good idea to
[submit an issue](https://github.com/input-output-hk/cardano-sl/issues/new) to
discuss the change before plowing into writing code.

Please make sure your contributions adhere to our coding guidelines:

*  Code must adhere to the [Serokell Haskell Style Guide](https://github.com/serokell/serokell-util/blob/master/serokell-style.md).
*  Code must be documented with [Haddock](https://www.haskell.org/haddock/doc/html/index.html).
*  We are using [GitFlow](http://nvie.com/posts/a-successful-git-branching-model/.)
   branching model, so pull requests need to be based on and opened against the `develop`
   branch.
*  Please refer to [this guide](https://chris.beams.io/posts/git-commit/) to write a good Git commit message.

Please note that Cardano SL uses a custom prelude [Universum](https://github.com/serokell/universum)
instead of the default one.

### Development Tricks

Common tasks for development are kept in `Makefile`s, one per package and one for the whole project.
Run `make help` to get assistance on custom commands.
As an example, you can run `make ghcid-test` in the `wallet-new` package to get a test-running `ghcid` process running.

### Code Quality

Cardano SL uses [HLint](https://github.com/ndmitchell/hlint) as a code quality tool.

You can install it using `stack install hlint` command.

To check Cardano SL code run this script (from the `cardano-sl` root directory):

```
$ ./scripts/haskell/lint.sh
```

### Code Style

Cardano SL uses [stylish-haskell](https://github.com/jaspervdj/stylish-haskell) tool to
prettify Haskell code.

Please note that there is `.stylish-haskell.yaml` in the root of the repository. This
configuration file requires `stylish-haskell` version `0.8.1.0` or newer.

You can install it using `stack install stylish-haskell` command.

We also use [`editorconfig`](https://editorconfig.org/) to maintain consistent indentation and maximum line length.
You can [download a plugin](https://editorconfig.org/#download) for almost any common editor.

## Documentation

Cardano SL Documentation is published at [cardanodocs.com](https://cardanodocs.com).

Please note that we have a [separate repository for documentation](https://github.com/input-output-hk/cardanodocs.com/).
So if you would like to help with documentation, please [submit a pull request](https://github.com/input-output-hk/cardanodocs.com/pulls)
with your changes/additions.

## Testing

To run tests for Cardano SL code use this command (from the `cardano-sl` root directory):

```
$ stack test
```
