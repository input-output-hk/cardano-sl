{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}

-- | Provides functionality of representing `Bi` instances as correct
-- `Message`s used by time-warp.

module Pos.Binary.Network () where

import qualified Data.Binary      as Binary
import           Pos.Binary.Class (Bi (..))
import           Pos.DHT.Class    (DHTMsgHeader (..))
import           Pos.DHT.Types    (DHTData, DHTKey (..))

-- TODO Rewrite :(((
instance Binary.Binary DHTMsgHeader

instance Bi DHTMsgHeader where
    put = Binary.put
    get = Binary.get

instance Binary.Binary DHTKey

instance Bi DHTKey where
    put = Binary.put
    get = Binary.get

instance Binary.Binary DHTData

instance Bi DHTData where
    put = Binary.put
    get = Binary.get
