{-# LANGUAGE ScopedTypeVariables #-}

-- | Leaders part of LRC DB.

module Pos.DB.Lrc.Leaders
       (
         -- * Getters
         getLeaders

       -- * Operations
       , putLeaders

       -- * Initialization
       , prepareLrcLeaders
       ) where

import           Universum

import           Pos.Binary.Class      (encodeStrict)
import           Pos.Binary.Types      ()
import           Pos.Context.Class     (WithNodeContext)
import           Pos.Context.Functions (genesisLeadersM)
import           Pos.DB.Class          (MonadDB)
import           Pos.DB.Lrc.Common     (getBi, putBi)
import           Pos.Types             (EpochIndex, SlotLeaders)

----------------------------------------------------------------------------
-- Getters
----------------------------------------------------------------------------

getLeaders :: MonadDB ssc m => EpochIndex -> m (Maybe SlotLeaders)
getLeaders = getBi . leadersKey

----------------------------------------------------------------------------
-- Operations
----------------------------------------------------------------------------

putLeaders :: MonadDB ssc m => EpochIndex -> SlotLeaders -> m ()
putLeaders epoch = putBi (leadersKey epoch)

----------------------------------------------------------------------------
-- Initialization
----------------------------------------------------------------------------

prepareLrcLeaders
    :: forall ssc m.
       (WithNodeContext ssc m, MonadDB ssc m)
    => m ()
prepareLrcLeaders = putIfEmpty (getLeaders 0) (putLeaders 0 =<< genesisLeadersM)
  where
    putIfEmpty
        :: forall a.
           (m (Maybe a)) -> m () -> m ()
    putIfEmpty getter putter = maybe putter (const pass) =<< getter

----------------------------------------------------------------------------
-- Keys
----------------------------------------------------------------------------

leadersKey :: EpochIndex -> ByteString
leadersKey = mappend "l/" . encodeStrict
