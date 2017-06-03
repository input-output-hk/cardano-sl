-- | We want to store each epoch's seed in the database. This module does
-- that. It's done so that we'd be able to reuse previous epoch's seed in
-- case we can't generate a seed for current epoch (see [CSL-50]).
module Pos.Lrc.DB.Seed
       ( -- * Operations
         getSeed
       , putSeed

         -- * Initialization
       , prepareLrcSeed
       ) where

import           Universum

import           Pos.Binary.Class  (encodeStrict)
import           Pos.Core.Types    (EpochIndex (..), SharedSeed)
import           Pos.DB.Class      (MonadRealDB)
import           Pos.Lrc.DB.Common (getBi, putBi)
import           Pos.Lrc.Genesis   (genesisSeed)

getSeed :: MonadRealDB m => EpochIndex -> m (Maybe SharedSeed)
getSeed epoch = getBi (seedKey epoch)

putSeed :: MonadRealDB m => EpochIndex -> SharedSeed -> m ()
putSeed epoch = putBi (seedKey epoch)

prepareLrcSeed :: MonadRealDB m => m ()
prepareLrcSeed =
    unlessM isInitialized $ putSeed (EpochIndex 0) genesisSeed

isInitialized :: MonadRealDB m => m Bool
isInitialized = isJust <$> getSeed (EpochIndex 0)

----------------------------------------------------------------------------
-- Keys
----------------------------------------------------------------------------

seedKey :: EpochIndex -> ByteString
seedKey = mappend "s/" . encodeStrict
