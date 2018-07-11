-- | Issuers part of LRC DB.

module Pos.Lrc.DB.Issuers
       ( IssuersStakes
         -- * Getters
       , getIssuersStakes

         -- * Operations
       , putIssuersStakes

         -- * Initialization
       , prepareLrcIssuers
       ) where

import           Universum

import           Pos.Binary.Class (serialize')
import           Pos.Core (CoreConfiguration)
import           Pos.Core.Common (Coin, StakeholderId)
import           Pos.Core.Slotting (EpochIndex (..))
import           Pos.DB.Class (MonadDB, MonadDBRead)
import           Pos.DB.Error (DBError (DBMalformed))
import           Pos.Lrc.DB.Common (getBi, putBi)
import           Pos.Util.Util (maybeThrow)

-- | The first value here is epoch for which this stake distribution is valid.
-- The second one is total stake corresponding to that epoch.
-- The third one is map which stores stake belonging to issuer of some block as
-- per epoch from the first value.
type IssuersStakes = HashMap StakeholderId Coin

getIssuersStakes :: MonadDBRead m => CoreConfiguration -> EpochIndex -> m IssuersStakes
getIssuersStakes cc epoch =
    maybeThrow (DBMalformed "Issuers part of LRC DB is not initialized") =<<
    getBi cc (issuersKey epoch)

putIssuersStakes :: MonadDB m => CoreConfiguration -> EpochIndex -> IssuersStakes -> m ()
putIssuersStakes cc epoch = putBi cc (issuersKey epoch)

prepareLrcIssuers :: MonadDB m => CoreConfiguration -> Coin -> m ()
prepareLrcIssuers cc _ =
    unlessM (isInitialized cc)
            (putIssuersStakes cc (EpochIndex 0) mempty)

isInitialized :: MonadDB m => CoreConfiguration -> m Bool
isInitialized cc = (isJust @IssuersStakes) <$> getBi cc (issuersKey $ EpochIndex 0)

----------------------------------------------------------------------------
-- Keys
----------------------------------------------------------------------------

issuersKey :: EpochIndex -> ByteString
issuersKey = mappend "i/" . serialize'
