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

import           Control.Lens          ((^.))
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
import           Pos.Types.Types       (EpochIndex, NEBlocks, SharedSeed, epochIndexL)
import           Pos.Util              (inAssertMode, _neHead)

class Monad m => MonadSscGS ssc m | m -> ssc where
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
       MonadSscGS ssc m
    => Reader (SscGlobalState ssc) a -> m a
sscRunGlobalQuery query = runReader query <$> getGlobalState @ssc

sscRunGlobalModify
    :: forall ssc m a .
    MonadSscGS ssc m
    => State (SscGlobalState ssc) a -> m a
sscRunGlobalModify upd = modifyGlobalState $ runState upd

-- sscRunImpureQuery
--     :: forall ssc m a.
--        (MonadSscGS ssc m)
--     => ReaderT (SscGlobalState ssc) m a -> m a
-- sscRunImpureQuery query = runReaderT query =<< getGlobalState @ssc

sscCalculateSeed
    :: forall ssc m.
       (MonadSscGS ssc m, SscStorageClass ssc)
    => EpochIndex -> m (Either (SscSeedError ssc) SharedSeed)
sscCalculateSeed = sscRunGlobalQuery . sscCalculateSeedM @ssc

sscApplyBlocks
    :: forall ssc m.
       (MonadSscGS ssc m, SscStorageClass ssc, WithLogger m)
    => NEBlocks ssc -> m ()
sscApplyBlocks blocks = do
    sscRunGlobalModify $ sscApplyBlocksM @ssc blocks
    gs <- getGlobalState @ssc
    inAssertMode $ do
        logDebug $ sformat ("After applying blocks SSC global state is:\n" %build) gs

sscRollback
    :: forall ssc m.
       (MonadSscGS ssc m, SscStorageClass ssc)
    => NEBlocks ssc -> m ()
sscRollback = sscRunGlobalModify . sscRollbackM @ssc

sscVerifyBlocks
    :: forall ssc m.
       ( MonadDB ssc m
       , MonadSscGS ssc m
       , WithNodeContext ssc m
       , SscStorageClass ssc
       )
    => Bool -> NEBlocks ssc -> m VerificationRes
sscVerifyBlocks verPure blocks = do
    let epoch = blocks ^. _neHead . epochIndexL
    richmen <- lrcActionOnEpochReason epoch
                   "couldn't get SSC richmen"
                   LrcDB.getRichmenSsc
    sscRunGlobalQuery $ sscVerifyBlocksM @ssc verPure richmen blocks
