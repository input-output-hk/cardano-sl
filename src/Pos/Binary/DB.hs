-- | Serialization of types defined in DB modules.

module Pos.Binary.DB
       (
       ) where

import           Data.Binary.Get       (label)
import           Universum

import           Pos.Binary.Class      (Bi (..))
import           Pos.DB.Types          (GtRichmenStorage (..), LeadersStorage (..),
                                        StoredBlock (..))
import           Pos.Ssc.Class.Helpers (SscHelpersClass)

instance SscHelpersClass ssc =>
         Bi (StoredBlock ssc) where
    put StoredBlock {..} = put sbBlock
    get = label "StoredBlock" $ StoredBlock <$> get

instance Bi (LeadersStorage ssc) where
    put LeadersStorage {..} = put lrcEpoch >> put lrcLeaders
    get = label "LeadersStorage" $ LeadersStorage <$> get <*> get

instance Bi (GtRichmenStorage ssc) where
    put GtRichmenStorage {..} = put gtRichmenEpoch >> put gtRichmen
    get = label "GtRichmenStorage" $ GtRichmenStorage <$> get <*> get
