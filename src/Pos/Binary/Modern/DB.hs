-- | Serialization of types defined in DB modules.

module Pos.Binary.Modern.DB
       (
       ) where

import           Universum

import           Pos.Binary.Class    (Bi (..))
import           Pos.Modern.DB.Types (StoredBlock (..))
import           Pos.Ssc.Class.Types (Ssc)

instance Ssc ssc =>
         Bi (StoredBlock ssc) where
    put StoredBlock {..} = put sbBlock >> put sbInMain
    get = StoredBlock <$> get <*> get
