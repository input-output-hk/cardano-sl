-- | Higher-level functionality of LRC DB.

module Pos.Lrc.DB.Lrc
       ( prepareLrcDB
       ) where

import           Universum

import           Pos.Core (SlotCount)
import           Pos.DB.Class (MonadDB)
import           Pos.DB.Error (DBError (..))
import           Pos.Lrc.DB.Common (prepareLrcCommon)
import           Pos.Lrc.DB.Issuers (prepareLrcIssuers)
import           Pos.Lrc.DB.Leaders (prepareLrcLeaders)
import           Pos.Lrc.DB.Richmen (prepareLrcRichmen, tryGetUSRichmen)
import           Pos.Lrc.DB.Seed (prepareLrcSeed)

import           Pos.Util (maybeThrow)

-- | Put missing initial data into LRC DB.
prepareLrcDB :: MonadDB m => SlotCount -> m ()
prepareLrcDB epochSlots = do
    prepareLrcLeaders epochSlots
    prepareLrcRichmen
    let cantReadErr =
            DBMalformed "Can't read richmen US after richmen initialization"
    totalStake <- fst <$> (maybeThrow cantReadErr =<< tryGetUSRichmen 0)
    prepareLrcIssuers totalStake
    prepareLrcSeed
    prepareLrcCommon
