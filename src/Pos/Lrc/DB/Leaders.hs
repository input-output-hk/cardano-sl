{-# LANGUAGE ScopedTypeVariables #-}

-- | Leaders part of LRC DB.

module Pos.Lrc.DB.Leaders
       (
         -- * Getters
         getLeaders

       -- * Operations
       , putLeaders

       -- * Initialization
       , prepareLrcLeaders
       ) where

import           Universum

import           EtherCompat

import           Pos.Binary.Class      (encodeStrict)
import           Pos.Binary.Core       ()
import           Pos.Context.Context   (GenesisStakes)
import           Pos.Context.Functions (genesisLeadersM)
import           Pos.DB.Class          (MonadDB, MonadDBRead)
import           Pos.Lrc.DB.Common     (getBi, putBi)
import           Pos.Types             (EpochIndex, SlotLeaders)

----------------------------------------------------------------------------
-- Getters
----------------------------------------------------------------------------

getLeaders :: MonadDBRead m => EpochIndex -> m (Maybe SlotLeaders)
getLeaders = getBi . leadersKey

----------------------------------------------------------------------------
-- Operations
----------------------------------------------------------------------------

putLeaders :: MonadDB m => EpochIndex -> SlotLeaders -> m ()
putLeaders epoch = putBi (leadersKey epoch)

----------------------------------------------------------------------------
-- Initialization
----------------------------------------------------------------------------

prepareLrcLeaders ::
       (MonadReader ctx m, HasLens GenesisStakes ctx GenesisStakes, MonadDB m)
    => m ()
prepareLrcLeaders =
    whenNothingM_ (getLeaders 0) $
        putLeaders 0 =<< genesisLeadersM

----------------------------------------------------------------------------
-- Keys
----------------------------------------------------------------------------

leadersKey :: EpochIndex -> ByteString
leadersKey = mappend "l/" . encodeStrict
