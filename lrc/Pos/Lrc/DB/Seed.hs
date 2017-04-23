-- | We want to store each epoch's seed in the database. This module does
-- that. It's done so that we'd be able to reuse previous epoch's seed in
-- case we can't generate a seed for current epoch (see [CSL-50]).
module Pos.Lrc.DB.Seed
       ( -- * Getters
         getSeed

         -- * Operations
       , putSeed

         -- * Initialization
       , prepareLrcSeed
       ) where

import           Universum

import           Pos.Binary.Class  (encodeStrict)
import           Pos.Core.Types    (EpochIndex (..), SharedSeed)
import           Pos.DB.Class      (MonadDB)
import           Pos.DB.Error      (DBError (DBMalformed))
import           Pos.Lrc.DB.Common (getBi, putBi)
import           Pos.Lrc.Genesis   (genesisSeed)
import           Pos.Util.Util     (maybeThrow)

getSeed :: MonadDB m => EpochIndex -> m SharedSeed
getSeed epoch =
    maybeThrow (DBMalformed "Seeds part of LRC DB is not initialized") =<<
    getBi (seedKey epoch)

putSeed :: MonadDB m => EpochIndex -> SharedSeed -> m ()
putSeed epoch = putBi (seedKey epoch)

prepareLrcSeed :: MonadDB m => m ()
prepareLrcSeed =
    unlessM isInitialized $ putSeed (EpochIndex 0) genesisSeed

isInitialized :: MonadDB m => m Bool
isInitialized = (isJust @(Maybe SharedSeed)) <$> getBi (seedKey $ EpochIndex 0)

----------------------------------------------------------------------------
-- Keys
----------------------------------------------------------------------------

seedKey :: EpochIndex -> ByteString
seedKey = mappend "s/" . encodeStrict
