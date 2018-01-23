-- | Convert from Cardano types to the DSL
module UTxO.Reify (
    Reify(..)
  ) where

import Universum

import Pos.Core

import UTxO.Context
import UTxO.Translate

{-------------------------------------------------------------------------------
  Translate from Cardano types to the UTxO

  TODO: It may be useful to have separate monads for interpretation and
  reification. For now kept them the same for simplicity.

  NOTE: We don't use the alternative type signature

  > reify :: Interpreted a -> Translate a

  because, for example, the interpretation of 'Addr' is @(KeyPair, Address)@
  but when we reify we want to translate just the @Address@ (i.e., 'reify'
  may have less information than the output of 'interpret').
-------------------------------------------------------------------------------}

class Reify a where
  -- | The result of reification
  type Reified a :: *

  -- | Translate from Cardano to the DSL
  reify :: a -> Translate (Reified a)

-- | Reify an address
instance Reify Address where
  type Reified Address = Addr

  reify :: Address -> Translate Addr
  reify = asks . resolveAddress
