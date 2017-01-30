{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Higher-level logic of SSC independent of concrete SSC.

module Pos.Ssc.Extra.Logic
       (
         -- * Utilities
         sscRunLocalQuery
       , sscRunGlobalQuery

         -- * Seed calculation
       , sscCalculateSeed

         -- * Local Data
       , sscGetLocalPayload
       , sscNormalize
       , sscNormalizeRichmen

         -- * GState
       , sscApplyBlocks
       , sscRollbackBlocks
       , sscVerifyBlocks
       ) where

import           Control.Lens            (_Wrapped)
import           Control.Monad.Except    (MonadError)
import           Control.Monad.Trans     (MonadTrans)
import           Formatting              (build, sformat, (%))
import           System.Wlog             (WithLogger, logDebug)
import           Universum

import           Pos.Context             (WithNodeContext, lrcActionOnEpochReason)
import           Pos.DB                  (MonadDB)
import qualified Pos.DB.Lrc              as LrcDB
import           Pos.Ssc.Class.LocalData (SscLocalDataClass (..))
import           Pos.Ssc.Class.Storage   (SscStorageClass (..))
import           Pos.Ssc.Class.Types     (Ssc (..))
import           Pos.Ssc.Extra.Class     (MonadSscMem)
import           Pos.Types               (Block, EpochIndex, SharedSeed, SlotId,
                                          epochIndexL)
import           Pos.Util                (NE, NewestFirst, OldestFirst, inAssertMode,
                                          _neHead)

----------------------------------------------------------------------------
-- Utilities
----------------------------------------------------------------------------

sscRunLocalQuery
    :: forall ssc m a.
       MonadSscMem ssc m
    => Reader (SscLocalData ssc) a -> m a
sscRunLocalQuery = notImplemented

sscRunGlobalQuery
    :: forall ssc m a.
       (MonadSscMem ssc m)
    => ReaderT (SscGlobalState ssc) m a -> m a
sscRunGlobalQuery = notImplemented

----------------------------------------------------------------------------
-- Seed calculation
----------------------------------------------------------------------------

sscCalculateSeed
    :: forall ssc m.
       (MonadSscMem ssc m, SscStorageClass ssc, MonadIO m)
    => EpochIndex -> m (Either (SscSeedError ssc) SharedSeed)
sscCalculateSeed = notImplemented

----------------------------------------------------------------------------
-- Local Data
----------------------------------------------------------------------------

sscGetLocalPayload
    :: forall ssc m.
       (MonadIO m, MonadSscMem ssc m, SscLocalDataClass ssc)
    => SlotId -> m (Maybe (SscPayload ssc))
sscGetLocalPayload neededSlot = notImplemented

sscNormalize
    :: forall ssc m.
       (MonadIO m, MonadSscMem ssc m, SscLocalDataClass ssc)
    => m ()
sscNormalize = notImplemented

-- MonadDB is needed to get richmen.
sscNormalizeRichmen
    :: forall ssc m.
       (MonadDB ssc m, MonadSscMem ssc m, SscLocalDataClass ssc)
    => EpochIndex -> m ()
sscNormalizeRichmen = notImplemented

----------------------------------------------------------------------------
-- GState
----------------------------------------------------------------------------

-- 'MonadIO' (part of 'MonadDB')  is needed only for 'TVar'.
-- 'MonadDB' is needed only to get richmen.
-- We can try to eliminate these constraints later.
type SscGlobalApplyMode ssc m =
    (MonadSscMem ssc m, SscStorageClass ssc, WithLogger m, MonadDB ssc m)
type SscGlobalVerifyMode ssc m =
    (MonadSscMem ssc m, SscStorageClass ssc, WithLogger m,
     MonadDB ssc m, MonadError (SscVerifyError ssc) m)

sscApplyBlocks
    :: forall ssc m.
       SscGlobalApplyMode ssc m
    => OldestFirst NE (Block ssc) -> Maybe (SscGlobalState ssc) -> m ()
sscApplyBlocks = notImplemented

sscRollbackBlocks
    :: forall ssc m.
       SscGlobalApplyMode ssc m
    => NewestFirst NE (Block ssc) -> m ()
sscRollbackBlocks = notImplemented

sscVerifyBlocks
    :: forall ssc m.
       SscGlobalVerifyMode ssc m
    => OldestFirst NE (Block ssc) -> m (SscGlobalState ssc)
sscVerifyBlocks = notImplemented
