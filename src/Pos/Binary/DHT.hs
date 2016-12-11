{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}

-- | Provides functionality of representing `Bi` instances as correct
-- `Message`s used by time-warp.

module Pos.Binary.DHT () where

import           Control.Monad.Fail (fail)
import           Data.Binary.Get    (getWord8)
import           Data.Binary.Put    (putWord8)
import           Universum

import           Pos.Binary.Class   (Bi (..))
import           Pos.DHT.Class      (DHTMsgHeader (..))
import           Pos.DHT.Types      (DHTData (..), DHTKey (..))

instance Bi DHTMsgHeader where
    put BroadcastHeader  = putWord8 0
    put (SimpleHeader b) = putWord8 1 >> put b
    get = getWord8 >>= \case
        0 -> pure BroadcastHeader
        1 -> SimpleHeader <$> get
        tag -> fail ("get@DHTMsgHeader: invalid tag: " ++ show tag)

instance Bi DHTKey where
    put (DHTKey bs) = put bs
    get = DHTKey <$> get

instance Bi DHTData where
    put (DHTData ()) = mempty
    get = pure $ DHTData ()
