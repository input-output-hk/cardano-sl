-- | Higher-level functionality of LRC DB.

module Pos.Lrc.DB.Lrc
       ( prepareLrcDB
       ) where

import           Universum

import           Pos.Core (SlotCount, CoreConfiguration, ProtocolConstants,
                     HasGenesisBlockVersionData, GenesisData)
import           Pos.DB.Class (MonadDB)
import           Pos.DB.Error (DBError (..))
import           Pos.Lrc.DB.Common (prepareLrcCommon)
import           Pos.Lrc.DB.Issuers (prepareLrcIssuers)
import           Pos.Lrc.DB.Leaders (prepareLrcLeaders)
import           Pos.Lrc.DB.Richmen (prepareLrcRichmen, tryGetUSRichmen)
import           Pos.Lrc.DB.Seed (prepareLrcSeed)

import           Pos.Util (maybeThrow)

-- | Put missing initial data into LRC DB.
prepareLrcDB
  :: (MonadDB m, HasGenesisBlockVersionData)
  => CoreConfiguration -> GenesisData -> ProtocolConstants -> SlotCount -> m ()
prepareLrcDB cc gd pd epochSlots = do
    prepareLrcLeaders cc gd pd epochSlots
    prepareLrcRichmen cc gd
    let cantReadErr =
            DBMalformed "Can't read richmen US after richmen initialization"
    totalStake <- fst <$> (maybeThrow cantReadErr =<< tryGetUSRichmen cc 0)
    prepareLrcIssuers cc totalStake
    prepareLrcSeed cc gd
    prepareLrcCommon cc
