-- | Issuers part of LRC DB.

module Pos.DB.Lrc.Issuers
       ( IssuersStakes
         -- * Getters
       , getIssuersStakes

         -- * Operations
       , putIssuersStakes

         -- * Initialization
       , prepareLrcIssuers
       ) where

import           Universum

import           Pos.Binary.Class  (encodeStrict)
import           Pos.DB.Class      (MonadDB)
import           Pos.DB.Error      (DBError (DBMalformed))
import           Pos.DB.Lrc.Common (getBi, putBi)
import           Pos.Types         (Coin, EpochIndex, StakeholderId, EpochIndex (..))
import           Pos.Util          (maybeThrow)

-- | The first value here is epoch for which this stake distribution is valid.
-- The second one is total stake corresponding to that epoch.
-- The third one is map which stores stake belonging to issuer of some block as
-- per epoch from the first value.
type IssuersStakes = HashMap StakeholderId Coin

getIssuersStakes :: MonadDB ssc m => EpochIndex -> m IssuersStakes
getIssuersStakes epoch =
    maybeThrow (DBMalformed "Issuers part of LRC DB is not initialized") =<<
    getBi (issuersKey epoch)

putIssuersStakes :: MonadDB ssc m => EpochIndex -> IssuersStakes -> m ()
putIssuersStakes epoch = putBi (issuersKey epoch)

prepareLrcIssuers :: MonadDB ssc m => Coin -> m ()
prepareLrcIssuers _ =
    unlessM isInitialized $ putIssuersStakes (EpochIndex 0) mempty

isInitialized :: MonadDB ssc m => m Bool
isInitialized = (isJust @(Maybe IssuersStakes)) <$> getBi (issuersKey $ EpochIndex 0)

----------------------------------------------------------------------------
-- Keys
----------------------------------------------------------------------------

issuersKey :: EpochIndex -> ByteString
issuersKey = mappend "i/" . encodeStrict
