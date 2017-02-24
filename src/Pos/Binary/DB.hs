-- | Serialization of types defined in DB modules.

module Pos.Binary.DB
       (
       ) where

import           Data.Binary.Get  (label)
import           Universum

import           Pos.Binary.Class (Bi (..))
import           Pos.DB.Lrc.Types (GtRichmenStorage (..), LeadersStorage (..))

instance Bi (LeadersStorage ssc) where
    put LeadersStorage {..} = put lrcEpoch >> put lrcLeaders
    get = label "LeadersStorage" $ LeadersStorage <$> get <*> get

instance Bi (GtRichmenStorage ssc) where
    put GtRichmenStorage {..} = put gtRichmenEpoch >> put gtRichmen
    get = label "GtRichmenStorage" $ GtRichmenStorage <$> get <*> get
