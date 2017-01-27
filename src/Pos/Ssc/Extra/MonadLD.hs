{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Type class to work with SscLocalData.

module Pos.Ssc.Extra.MonadLD
       ( MonadSscLD (..)
       , sscApplyGlobalState
       , sscGetLocalPayload
       , sscRunLocalQuery
       , sscRunLocalUpdate
       ) where

import           Control.Concurrent.STM  (TVar, readTVar)
import           Control.Monad.Except    (ExceptT)
import           Control.Monad.Trans     (MonadTrans)
import           Universum

import           Pos.DHT.Real            (KademliaDHT)
import           Pos.Lrc.Types           (Richmen)
import           Pos.Ssc.Class.LocalData (SscLocalDataClass (..))
import           Pos.Ssc.Class.Types     (Ssc (..))
import           Pos.Types               (SlotId)

class Monad m => MonadSscLD ssc m | m -> ssc where
    askSscLD :: m (TVar (SscLocalData ssc))
    getLocalData :: m (SscLocalData ssc)
    setLocalData :: SscLocalData ssc -> m ()
    modifyLocalData :: ((SscGlobalState ssc, SscLocalData ssc)
                     -> (a, SscLocalData ssc)) -> m a

    default askSscLD :: (MonadTrans t, MonadSscLD ssc m', t m' ~ m) => m (TVar (SscLocalData ssc))
    askSscLD = lift askSscLD

    default getLocalData :: (MonadTrans t, MonadSscLD ssc m', t m' ~ m) => m (SscLocalData ssc)
    getLocalData = lift getLocalData

    default setLocalData :: (MonadTrans t, MonadSscLD ssc m', t m' ~ m) => SscLocalData ssc -> m ()
    setLocalData = lift . setLocalData

    default modifyLocalData :: (MonadTrans t, MonadSscLD ssc m', t m' ~ m) => ((SscGlobalState ssc, SscLocalData ssc)
                     -> (a, SscLocalData ssc)) -> m a
    modifyLocalData = lift . modifyLocalData

instance (Monad m, MonadSscLD ssc m) => MonadSscLD ssc (ReaderT x m)
instance (Monad m, MonadSscLD ssc m) => MonadSscLD ssc (StateT x m)
instance (Monad m, MonadSscLD ssc m) => MonadSscLD ssc (ExceptT x m)
instance (Monad m, MonadSscLD ssc m) => MonadSscLD ssc (KademliaDHT m)

-- | Convenient wrapper to run LocalQuery in MonadSscLD.
sscRunLocalQuery
    :: forall ssc m a.
       MonadSscLD ssc m
    => Reader (SscLocalData ssc) a -> m a
sscRunLocalQuery query = runReader query <$> getLocalData @ssc

-- | Convenient wrapper to run LocalUpdate in MonadSscLD.
sscRunLocalUpdate
    :: MonadSscLD ssc m
    => State (SscLocalData ssc) a -> m a
sscRunLocalUpdate upd =
    modifyLocalData (\(_, l) -> runState upd l)

sscGetLocalPayload
    :: forall ssc m.
       (MonadIO m, MonadSscLD ssc m, SscLocalDataClass ssc)
    => SlotId -> m (Maybe (SscPayload ssc))
sscGetLocalPayload neededSlot = do
    ldVar <- askSscLD
    atomically $ do
        ld <- readTVar ldVar
        let (slot, payload) = runReader (sscGetLocalPayloadQ @ssc) ld
        if | slot == neededSlot -> return (Just payload)
           | slot < neededSlot -> retry
           | otherwise -> return Nothing

sscApplyGlobalState
    :: forall ssc m.
       (MonadSscLD ssc m, SscLocalDataClass ssc)
    => Richmen -> m ()
sscApplyGlobalState richmen =
    modifyLocalData (\(gs, ld) -> runState (sscApplyGlobalStateU @ssc richmen gs) ld)
