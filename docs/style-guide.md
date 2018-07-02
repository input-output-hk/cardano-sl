# Haskell Style Guide

There are really only 5 things you need to know:

1. Use 4 spaces.
2. Use [`stylish-haskell`](https://github.com/jaspervdj/stylish-haskell) for imports.
3. Use spaces around all operators in all cases (exception: `(%)` from `Formatting`).
4. As a default, use `!` to make all fields in `data` types strict. If strictness annotations are omitted, document why.
5. As a default, make functions lazy. If they must be strict, document why.
6. All imports should be either qualified or explicit, i.e. with enumeration of stuff that you're importing (exception: `Universum`).

The rest of the style guide can be found [here](https://github.com/serokell/serokell-core/blob/master/serokell-style.md).
