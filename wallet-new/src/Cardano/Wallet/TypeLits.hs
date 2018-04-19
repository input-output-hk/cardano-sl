
module Cardano.Wallet.TypeLits where

import           Universum

import           GHC.TypeLits (KnownSymbol, Symbol, symbolVal)

-- | Shamelessly copied from:
-- <https://stackoverflow.com/questions/37364835/how-to-get-the-type-level-values-of-string-in-haskell>
-- The idea is to extend `KnownSymbol` to a type-level list, so that it's possibly to reify at the value-level
-- a `'[Symbol]` into a `[String]`.
class KnownSymbols (xs :: [Symbol]) where
  symbolVals :: proxy xs -> [String]

instance KnownSymbols ('[]) where
  symbolVals _ = []

instance (KnownSymbol a, KnownSymbols as) => KnownSymbols (a ': as) where
  symbolVals _ =
    symbolVal (Proxy :: Proxy a) : symbolVals (Proxy :: Proxy as)
