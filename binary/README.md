# cardano-sl-binary

Binary serialisation and deserialisation for Cardano SL.

This library is built on top of the [binary] and [cborg] packages and implement
space efficient serialising and deserialising of Haskell data types to and from
binary representations via a `Bi` type class and associated machinery.

The library includes ability the derive `Bi` instances (requires a `Typeable`
constraint).

[binary]: https://hackage.haskell.org/package/binary
[cborg]: https://hackage.haskell.org/package/cborg
