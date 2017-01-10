{-# LANGUAGE UndecidableInstances #-}

-- | Provides functionality of representing `Bi` instances as correct
-- `Message`s used by time-warp.

module Pos.Binary.NewDHTModel () where

import           Universum

import           Pos.Binary.Class       (Bi (..))
import           Pos.NewDHT.Model.Types (DHTData (..), DHTKey (..))


instance Bi DHTKey where
    put (DHTKey bs) = put bs
    get = DHTKey <$> get

instance Bi DHTData where
    put (DHTData ()) = mempty
    get = pure $ DHTData ()
