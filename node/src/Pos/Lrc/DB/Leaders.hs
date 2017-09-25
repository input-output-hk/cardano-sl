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

import           Pos.Binary.Class      (serialize')
import           Pos.Binary.Core       ()
import           Pos.Context.Functions (genesisLeaders)
import           Pos.Core              (EpochIndex, HasConfiguration, SlotLeaders)
import           Pos.DB.Class          (MonadDB, MonadDBRead)
import           Pos.Lrc.DB.Common     (getBi, putBi)

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
       ( MonadDB m
       , HasConfiguration
       )
    => m ()
prepareLrcLeaders =
    whenNothingM_ (getLeaders 0) $
        putLeaders 0 genesisLeaders

----------------------------------------------------------------------------
-- Keys
----------------------------------------------------------------------------

leadersKey :: EpochIndex -> ByteString
leadersKey = mappend "l/" . serialize'
