module Pos.Core.Lenses.Types where

import Prelude as Prelude
import Data.Lens as Lens
import Data.Either as Either
import Pos.Core.Types


getCoin :: forall a b r. Lens.Lens { "getCoin" :: a | r } { "getCoin" :: b | r } a b
getCoin = Lens.lens _."getCoin" (_ { "getCoin" = _ })

_Coin :: Lens.Iso' Coin
           { "getCoin" :: Int
           }
_Coin = Lens.iso unwrap Coin
  where
    unwrap (Coin x) = x

getEpochIndex :: forall a b r. Lens.Lens { "getEpochIndex" :: a | r } { "getEpochIndex" :: b | r } a b
getEpochIndex = Lens.lens _."getEpochIndex" (_ { "getEpochIndex" = _ })

_EpochIndex :: Lens.Iso' EpochIndex
                 { "getEpochIndex" :: Int
                 }
_EpochIndex = Lens.iso unwrap EpochIndex
  where
    unwrap (EpochIndex x) = x

getSlotIndex :: forall a b r. Lens.Lens { "getSlotIndex" :: a | r } { "getSlotIndex" :: b | r } a b
getSlotIndex = Lens.lens _."getSlotIndex" (_ { "getSlotIndex" = _ })

_LocalSlotIndex :: Lens.Iso' LocalSlotIndex
                     { "getSlotIndex" :: Int
                     }
_LocalSlotIndex = Lens.iso unwrap LocalSlotIndex
  where
    unwrap (LocalSlotIndex x) = x
