-- | Serialization of types defined in DB modules.

module Pos.Binary.DB
       (
       ) where

import           Universum

import           Pos.Binary.Class    (Bi (..))
import           Pos.DB.Types        (LeadersStorage (..), StoredBlock (..))
import           Pos.Ssc.Class.Types (Ssc)

instance Ssc ssc =>
         Bi (StoredBlock ssc) where
    put StoredBlock {..} = put sbBlock >> put sbInMain
    get = StoredBlock <$> get <*> get

instance Bi (LeadersStorage ssc) where
    put LeadersStorage {..} = put lrcEpoch >> put lrcLeaders
    get = LeadersStorage <$> get <*> get
