# Haskell Style Guide

There are really only 5 things you need to know:

1. Use 4 spaces.
2. Use [`stylish-haskell`](https://github.com/jaspervdj/stylish-haskell) for imports.
3. Use spaces around all operators in all cases (exception: `(%)` from `Formatting`).
4. Use `!` for all fields in `data` types.
5. All imports should be either qualified or explicit, i.e. with enumeration of stuff that you're importing (exceptions: `Universum` and `Prelude`).

The rest of the style guide can be found [here](https://github.com/serokell/serokell-core/blob/master/serokell-style.md).
