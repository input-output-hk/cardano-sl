{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Type class to work with SscGlobalState.

module Pos.Ssc.Extra.MonadGS
       ( MonadSscGS (..)
       , sscRunGlobalQuery
       , sscRunGlobalModify

       , sscApplyBlocks
       , sscCalculateSeed
       , sscRollback
       , sscVerifyBlocks
       ) where

import           Control.Lens          (_Wrapped)
import           Control.Monad.Except  (ExceptT)
import           Control.Monad.Trans   (MonadTrans)
import           Formatting            (build, sformat, (%))
import           Serokell.Util         (VerificationRes)
import           System.Wlog           (WithLogger, logDebug)
import           Universum

import           Pos.Context           (WithNodeContext, lrcActionOnEpochReason)
import           Pos.DB                (MonadDB)
import qualified Pos.DB.Lrc            as LrcDB
import           Pos.Ssc.Class.Storage (SscStorageClass (..))
import           Pos.Ssc.Class.Types   (Ssc (..))
import           Pos.Types             (Block, EpochIndex, SharedSeed, epochIndexL)
import           Pos.Util              (NE, NewestFirst, OldestFirst, inAssertMode,
                                        _neHead)

class WithLogger m => MonadSscGS ssc m | m -> ssc where
    getGlobalState    :: m (SscGlobalState ssc)
    setGlobalState    :: SscGlobalState ssc -> m ()
    modifyGlobalState :: (SscGlobalState ssc -> (a, SscGlobalState ssc)) -> m a

    default getGlobalState :: (MonadTrans t, MonadSscGS ssc m', t m' ~ m) => m (SscGlobalState ssc)
    getGlobalState = lift getGlobalState

    default setGlobalState :: (MonadTrans t, MonadSscGS ssc m', t m' ~ m) => SscGlobalState ssc -> m ()
    setGlobalState = lift . setGlobalState

    default modifyGlobalState :: (MonadTrans t, MonadSscGS ssc m', t m' ~ m) =>
                                 (SscGlobalState ssc -> (a, SscGlobalState ssc)) -> m a
    modifyGlobalState = lift . modifyGlobalState

instance MonadSscGS ssc m => MonadSscGS ssc (ReaderT a m) where
instance MonadSscGS ssc m => MonadSscGS ssc (StateT a m) where
instance MonadSscGS ssc m => MonadSscGS ssc (ExceptT a m) where

sscRunGlobalQuery
    :: forall ssc m a.
       (WithLogger m, MonadSscGS ssc m)
    => ReaderT (SscGlobalState ssc) m a -> m a
sscRunGlobalQuery query = runReaderT query =<< getGlobalState @ssc

sscRunGlobalModify
    :: forall ssc m a .
    (MonadSscGS ssc m)
    => State (SscGlobalState ssc) a -> m a
sscRunGlobalModify upd = modifyGlobalState $ runState upd

sscCalculateSeed
    :: forall ssc m.
       (MonadSscGS ssc m, SscStorageClass ssc)
    => EpochIndex -> m (Either (SscSeedError ssc) SharedSeed)
sscCalculateSeed = sscRunGlobalQuery . sscCalculateSeedM @ssc

sscApplyBlocks
    :: forall ssc m.
       (MonadSscGS ssc m, SscStorageClass ssc, WithLogger m)
    => OldestFirst NE (Block ssc) -> m ()
sscApplyBlocks blocks = do
    sscRunGlobalModify $ sscApplyBlocksM @ssc blocks
    gs <- getGlobalState @ssc
    inAssertMode $ do
        logDebug $ sformat ("After applying blocks SSC global state is:\n" %build) gs

sscRollback
    :: forall ssc m.
       (MonadSscGS ssc m, SscStorageClass ssc)
    => NewestFirst NE (Block ssc) -> m ()
sscRollback = sscRunGlobalModify . sscRollbackM @ssc

-- | Invariant: all blocks have the same epoch.
sscVerifyBlocks
    :: forall ssc m.
       ( MonadDB ssc m
       , MonadSscGS ssc m
       , WithNodeContext ssc m
       , SscStorageClass ssc
       , WithLogger m
       )
    => Bool -> OldestFirst NE (Block ssc) -> m VerificationRes
sscVerifyBlocks verPure blocks = do
    let epoch = blocks ^. _Wrapped . _neHead . epochIndexL
    richmen <- lrcActionOnEpochReason epoch
                   "couldn't get SSC richmen"
                   LrcDB.getRichmenSsc
    sscRunGlobalQuery $ sscVerifyBlocksM @ssc verPure richmen blocks
