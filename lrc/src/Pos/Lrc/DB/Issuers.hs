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

getIssuersStakes :: MonadDBRead m => EpochIndex -> m IssuersStakes
getIssuersStakes epoch =
    maybeThrow (DBMalformed "Issuers part of LRC DB is not initialized") =<<
    getBi (issuersKey epoch)

putIssuersStakes :: MonadDB m => EpochIndex -> IssuersStakes -> m ()
putIssuersStakes epoch = putBi (issuersKey epoch)

prepareLrcIssuers :: MonadDB m => Coin -> m ()
prepareLrcIssuers _ =
    unlessM isInitialized $ putIssuersStakes (EpochIndex 0) mempty

isInitialized :: MonadDB m => m Bool
isInitialized = (isJust @IssuersStakes) <$> getBi (issuersKey $ EpochIndex 0)

----------------------------------------------------------------------------
-- Keys
----------------------------------------------------------------------------

issuersKey :: EpochIndex -> ByteString
issuersKey = mappend "i/" . serialize'
