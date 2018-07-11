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

import           Pos.Binary.Class (serialize')
import           Pos.Core (GenesisData, CoreConfiguration, EpochIndex (..),
                     SharedSeed, gdFtsSeed)
import           Pos.DB.Class (MonadDB, MonadDBRead)
import           Pos.Lrc.DB.Common (getBi, putBi)

getSeed :: MonadDBRead m => CoreConfiguration -> EpochIndex -> m (Maybe SharedSeed)
getSeed cc epoch = getBi cc (seedKey epoch)

putSeed :: MonadDB m => CoreConfiguration -> EpochIndex -> SharedSeed -> m ()
putSeed cc epoch = putBi cc (seedKey epoch)

prepareLrcSeed :: MonadDB m => CoreConfiguration -> GenesisData -> m ()
prepareLrcSeed cc gd =
    unlessM (isInitialized cc) $ putSeed cc (EpochIndex 0) (gdFtsSeed gd)

isInitialized :: MonadDBRead m => CoreConfiguration -> m Bool
isInitialized cc = isJust <$> getSeed cc (EpochIndex 0)

----------------------------------------------------------------------------
-- Keys
----------------------------------------------------------------------------

seedKey :: EpochIndex -> ByteString
seedKey = mappend "s/" . serialize'
