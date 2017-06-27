-- | Higher-level functionality of LRC DB.

module Pos.Lrc.DB.Lrc
       ( prepareLrcDB
       ) where

import           Universum

import           Ether.Internal      (HasLens (..))

import           Pos.Context.Context (GenesisStakes, GenesisUtxo)
import           Pos.DB.Class        (MonadDB)
import           Pos.DB.Error        (DBError (..))
import           Pos.Lrc.DB.Common   (prepareLrcCommon)
import           Pos.Lrc.DB.Issuers  (prepareLrcIssuers)
import           Pos.Lrc.DB.Leaders  (prepareLrcLeaders)
import           Pos.Lrc.DB.Richmen  (getRichmenUS, prepareLrcRichmen)
import           Pos.Lrc.DB.Seed     (prepareLrcSeed)
import           Pos.Util            (maybeThrow)

-- | Put missing initial data into LRC DB.
prepareLrcDB
    :: ( MonadReader ctx m
       , HasLens GenesisStakes ctx GenesisStakes
       , HasLens GenesisUtxo ctx GenesisUtxo
       , MonadDB m
       )
    => m ()
prepareLrcDB = do
    prepareLrcLeaders
    prepareLrcRichmen
    let cantReadErr =
            DBMalformed "Can't read richmen US after richmen initialization"
    totalStake <- fst <$> (maybeThrow cantReadErr =<< getRichmenUS 0)
    prepareLrcIssuers totalStake
    prepareLrcSeed
    prepareLrcCommon
