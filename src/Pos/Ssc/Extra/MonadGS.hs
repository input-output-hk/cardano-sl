{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DefaultSignatures      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}

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

import           Control.Monad.Except    (ExceptT)
import           Control.Monad.Trans     (MonadTrans)
import           Control.TimeWarp.Rpc    (ResponseT)
import           Serokell.Util           (VerificationRes)
import           Universum

import           Pos.Crypto              (Threshold)
import           Pos.DHT.Model.Class     (DHTResponseT)
import           Pos.DHT.Real            (KademliaDHT)
import           Pos.Ssc.Class.Helpers   (SscHelpersClassM (sscCalculateSeedQ))
import           Pos.Ssc.Class.Storage   (SscStorageClassM (..))
import           Pos.Ssc.Class.Types     (Ssc (..))
import           Pos.State.Storage.Types (AltChain)
import           Pos.Types.Types         (EpochIndex, SharedSeed)

class Monad m => MonadSscGS ssc m | m -> ssc where
    getGlobalState    :: m (SscGlobalStateM ssc)
    setGlobalState    :: SscGlobalStateM ssc -> m ()
    modifyGlobalState :: (SscGlobalStateM ssc -> (a, SscGlobalStateM ssc)) -> m a

    default getGlobalState :: MonadTrans t => t m (SscGlobalStateM ssc)
    getGlobalState = lift getGlobalState

    default setGlobalState :: MonadTrans t => SscGlobalStateM ssc -> t m ()
    setGlobalState = lift . setGlobalState

    default modifyGlobalState :: MonadTrans t =>
                                 (SscGlobalStateM ssc -> (a, SscGlobalStateM ssc)) -> t m a
    modifyGlobalState = lift . modifyGlobalState

instance MonadSscGS ssc m => MonadSscGS ssc (ReaderT a m) where
instance MonadSscGS ssc m => MonadSscGS ssc (ExceptT a m) where
instance MonadSscGS ssc m => MonadSscGS ssc (ResponseT s m) where
instance MonadSscGS ssc m => MonadSscGS ssc (DHTResponseT s m) where
instance MonadSscGS ssc m => MonadSscGS ssc (KademliaDHT m) where

sscRunGlobalQuery
    :: forall ssc m a.
       MonadSscGS ssc m
    => Reader (SscGlobalStateM ssc) a -> m a
sscRunGlobalQuery query = runReader query <$> getGlobalState @ssc

sscRunGlobalModify
    :: forall ssc m a .
    MonadSscGS ssc m
    => State (SscGlobalStateM ssc) a -> m a
sscRunGlobalModify upd = modifyGlobalState $ runState upd

sscCalculateSeed
    :: forall ssc m.
       (MonadSscGS ssc m, SscHelpersClassM ssc)
    => EpochIndex -> Threshold -> m (Either (SscSeedError ssc) SharedSeed)
sscCalculateSeed e = sscRunGlobalQuery . sscCalculateSeedQ @ssc e

sscApplyBlocks
    :: forall ssc m.
       (MonadSscGS ssc m, SscStorageClassM ssc)
    => AltChain ssc -> m ()
sscApplyBlocks = sscRunGlobalModify . sscApplyBlocksM @ssc

sscRollback
    :: forall ssc m.
       (MonadSscGS ssc m, SscStorageClassM ssc)
    => AltChain ssc -> m ()
sscRollback = sscRunGlobalModify . sscRollbackM @ssc

sscVerifyBlocks
    :: forall ssc m.
       (MonadSscGS ssc m, SscStorageClassM ssc)
    => AltChain ssc -> m VerificationRes
sscVerifyBlocks = sscRunGlobalQuery . sscVerifyBlocksM @ssc
