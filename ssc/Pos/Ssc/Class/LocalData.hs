{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes          #-}

-- | This module defines type class for local data storage.

module Pos.Ssc.Class.LocalData
       ( SscLocalDataTag
       , LocalQuery
       , LocalUpdate
       , SscLocalDataClass (..)
       ) where

import           Universum

import           System.Wlog         (WithLogger)

import           Pos.Core            (BlockVersionData, EpochIndex, HasCoreConstants,
                                      SlotId)
import           Pos.DB.Class        (MonadDBRead)
import           Pos.Lrc.Types       (RichmenStakes)
import           Pos.Slotting.Class  (MonadSlots)
import           Pos.Ssc.Class.Types (Ssc (..))
import           Pos.Util.Util       (HasLens)

----------------------------------------------------------------------------
-- Modern
----------------------------------------------------------------------------

data SscLocalDataTag

type LocalQuery ssc a
     = forall ctx m. ( WithLogger m
                     , MonadReader ctx m
                     , HasLens SscLocalDataTag ctx (SscLocalData ssc)
                     , HasCoreConstants ctx
                     ) => m a

type LocalUpdate ssc a
     = forall ctx m. ( MonadState (SscLocalData ssc) m
                     , WithLogger m
                     , MonadReader ctx m
                     , HasCoreConstants ctx
                     ) => m a

-- | This type class abstracts local data used for SSC. Local means
-- that it is not stored in blocks.
class Ssc ssc => SscLocalDataClass ssc where
    -- | Get local payload to be put into main block and for given
    -- 'SlotId'. If payload for given 'SlotId' can't be constructed,
    -- empty payload can be returned.
    sscGetLocalPayloadQ :: SlotId -> LocalQuery ssc (SscPayload ssc)
    -- | Make 'SscLocalData' valid for given epoch, richmen and global state.
    -- of best known chain).
    sscNormalizeU :: (EpochIndex, RichmenStakes)
                  -> BlockVersionData
                  -> SscGlobalState ssc
                  -> LocalUpdate ssc ()
    -- | Create new (empty) local data. We are using this function instead of
    -- 'Default' class, because it gives more flexibility. For instance, one
    -- can read something from DB or get current slot.
    sscNewLocalData :: (MonadSlots m, MonadDBRead m) => m (SscLocalData ssc)
