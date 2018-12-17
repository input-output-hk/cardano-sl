-- | Higher-level functionality of LRC DB.

module Pos.DB.Lrc.Lrc
       ( prepareLrcDB
       ) where

import           Universum

import           Pos.Chain.Genesis as Genesis (Config (..),
                     configBlockVersionData, configFtsSeed)
import           Pos.DB.Class (MonadDB)
import           Pos.DB.Error (DBError (..))
import           Pos.DB.Lrc.Common (prepareLrcCommon)
import           Pos.DB.Lrc.Issuers (prepareLrcIssuers)
import           Pos.DB.Lrc.Leaders (prepareLrcLeaders)
import           Pos.DB.Lrc.Richmen (prepareLrcRichmen, tryGetUSRichmen)
import           Pos.DB.Lrc.Seed (prepareLrcSeed)

import           Pos.Util (maybeThrow)

-- | Put missing initial data into LRC DB.
prepareLrcDB :: MonadDB m => Genesis.Config -> m ()
prepareLrcDB genesisConfig = do
    prepareLrcLeaders genesisConfig
    prepareLrcRichmen genesisConfig
    let cantReadErr =
            DBMalformed "Can't read richmen US after richmen initialization"
    totalStake <-
        fst
            <$> (   maybeThrow cantReadErr
                =<< tryGetUSRichmen (configBlockVersionData genesisConfig) 0
                )
    prepareLrcIssuers totalStake
    prepareLrcSeed (configFtsSeed genesisConfig)
    prepareLrcCommon
