# Code Organisation Guidelines

The document specifies how code in the Cardano SL git repository should be organised. Consistent
module naming schemes and file locations make the code relatively uniform so that it is easier for
developers (internal and external), engineers working on formal specifications and auditors
(internal and external) to navigate the code base.

Currently, the code base does not conform to these guidelines but it is hoped that over time it
can move from non-conforming to conforming. Making code conform to these guidelines in one big
pull request or one long lived refactoring branch would be a huge amount of work and/or cause a
lot of disruption to the regular flow of development. Instead, these guidelines should be applied
as a series of small easy-to-review commits that can be merged relatively quickly.

* All library code for a Haskell package should be in the `src` directory of that package.
  Currently some packages use `src` and others use `Pos`.  For example the `lib` package currently
  has testing related code in the `src/Test` directory that should be be moved to the `test`
  directory.

* The `cardano-sl` git repository includes a number of distinct Haskell packages like `core`, `util`,
  `explorer`, `networking` and so on. All Haskell modules within each of these packages should be
  given a unique name prefix. All modules within `core` would have a prefix `Pos.Core`, modules
  within `util` would have a name prefix `Pos.Util`, modules within `networking` might have a prefix
  of `Pos.Network` and so on. This means that when looking at the imports for a module, it is
  immediately obvious which package each import comes from. As an example of a file that needs to be
  renamed/moved the file `core/Pos/Aeson/Core.hs` should be renamed to `core/src/Pos/Core/Aeson.hs`
  so that if it is imported in another module its name `Pos.Core.Aeson` would immediately suggest
  its location in the source tree.

* Tests for a Haskell package should always be a part of the same package as the implementation of
  the code it is testing. Test code should not be included in the library that a package provides.
  All test code for a Haskell package should be in the `test` directory of that package and modules
  with testing related code should have module names staring with `Test` and then reflecting the
  Haskell package which the code is testing, so test code for the `core` package would have all its
  test module names begin with `Test.Pos.Core`. An example of where the current code base
  contravenes these guidelines is that `core` and `txp` currently don't define any tests, but at
  least some of the functionality in these two modules are tested in `lib` package.

* Arbitrary instances for testing should never be in the library source tree and should not be
  compiled into the library defined by the package. Instead, Arbitrary instances should be under the
  `test` directory as per the previous point. When Arbitrary instances or other testing helper
  functionality needs to be used by other packages, a test specific package should be created. For
  instance, the `core` package may have `Arbitrary` instances that are need for tests in the `txp`
  package. In this case, there will be a cabal file `core/test/cardano-sl-core-test.cabal` which
  exports the required modules, all of which would have a `Test.Pos.Core` name prefix.
  [PR #2588](https://github.com/input-output-hk/cardano-sl/pull/2588) is an example of doing this
  for the `crypto` package.

* The library that a Haskell package provides should never include or export any code that is
  not part of the API the library provides to clients of the library. It is however valid for
  a library to provide interface functions to its internals that are required for testing or
  benchmarking, thus becoming part of the library's API.

* Benchmarking code should be in a separate tree under the `bench` directory of the haskell package
  it is benchmarking. Modules that are solely required for benchmarking should be in the `bench`
  sub directory and have module names beginning with `Bench` and something related to the package
  they are a part of. For instance, benchmarking code in the `core` package should have names that
  start with `Bench.Core`. An example of code that contravenes part of this is the `networking`
  package which provides the module `Bench.Network.Commons` that is only used by the benchmarking
  target in the `src` tree and is also included as an exposed module in the `library` section of
  the cabal file.

* Example code should also be in a separate tree under the `example` directory of the current
  package. An example of code that contravenes this is the `networking` package which provides
  `NTP.Example`.
