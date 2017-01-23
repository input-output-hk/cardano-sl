-- | Issuers part of LRC DB.

module Pos.DB.Lrc.Issuers
       (
         -- * Getters
         getIssuersStakes

         -- * Operations
       , putIssuersStakes

         -- * Initialization
       , prepareLrcIssuers
       ) where

import           Universum

import           Pos.DB.Class      (MonadDB)
import           Pos.DB.Error      (DBError (DBMalformed))
import           Pos.DB.Lrc.Common (getBi, putBi)
import           Pos.Types         (Coin, EpochIndex, StakeholderId)
import           Pos.Util          (maybeThrow)

-- | The first value here is epoch for which this stake distribution is valid.
-- The second one is total stake corresponding to that epoch.
-- The third one is map which stores stake belonging to issuer of some block as
-- per epoch from the first value.
type IssuersStakes = (EpochIndex, Coin, HashMap StakeholderId Coin)

getIssuersStakes :: MonadDB ssc m => m IssuersStakes
getIssuersStakes =
    maybeThrow (DBMalformed "Issuers part of LRC DB is not initialized") =<<
    getBi issuersKey

putIssuersStakes :: MonadDB ssc m => IssuersStakes -> m ()
putIssuersStakes = putBi issuersKey

prepareLrcIssuers :: MonadDB ssc m => Coin -> m ()
prepareLrcIssuers genesisTotalStake =
    unlessM (isInitialized) $ putIssuersStakes (0, genesisTotalStake, mempty)

isInitialized :: MonadDB ssc m => m Bool
isInitialized = (isJust @(Maybe IssuersStakes)) <$> getBi issuersKey

----------------------------------------------------------------------------
-- Keys
----------------------------------------------------------------------------

issuersKey :: ByteString
issuersKey = "i/"
