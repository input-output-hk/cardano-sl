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

import qualified Ether

import           Pos.Binary.Class      (encodeStrict)
import           Pos.Binary.Core       ()
import           Pos.Context.Context   (GenesisLeaders)
import           Pos.Context.Functions (genesisLeadersM)
import           Pos.DB.Class          (MonadDB)
import           Pos.Lrc.DB.Common     (getBi, putBi)
import           Pos.Types             (EpochIndex, SlotLeaders)

----------------------------------------------------------------------------
-- Getters
----------------------------------------------------------------------------

getLeaders :: MonadDB m => EpochIndex -> m (Maybe SlotLeaders)
getLeaders = getBi . leadersKey

----------------------------------------------------------------------------
-- Operations
----------------------------------------------------------------------------

putLeaders :: MonadDB m => EpochIndex -> SlotLeaders -> m ()
putLeaders epoch = putBi (leadersKey epoch)

----------------------------------------------------------------------------
-- Initialization
----------------------------------------------------------------------------

prepareLrcLeaders :: (Ether.MonadReader' GenesisLeaders m, MonadDB m) => m ()
prepareLrcLeaders =
    whenNothingM_ (getLeaders 0) $
        putLeaders 0 =<< genesisLeadersM

----------------------------------------------------------------------------
-- Keys
----------------------------------------------------------------------------

leadersKey :: EpochIndex -> ByteString
leadersKey = mappend "l/" . encodeStrict
