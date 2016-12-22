{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Binary serialization of Pos.Types.Coin

module Pos.Binary.Coin () where

import           Data.Binary.Get  (getWord64be)
import           Data.Binary.Put  (putWord64be)
import           Universum

import           Pos.Binary.Class (Bi (..))
import           Pos.Types.Coin   (Coin (..))

instance Bi Coin where
    put = putWord64be . getCoin
    get = Coin <$> getWord64be
