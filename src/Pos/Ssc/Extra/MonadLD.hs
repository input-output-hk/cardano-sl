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

import           Control.Monad.Trans     (MonadTrans)
import           Universum

import           Pos.DHT.Model           (DHTResponseT)
import           Pos.DHT.Real            (KademliaDHT)
import           Pos.Ssc.Class.LocalData (SscLocalDataClass (..))
import           Pos.Ssc.Class.Types     (Ssc (..))
import           Pos.Types.Types         (SlotId)

class Monad m => MonadSscLD ssc m | m -> ssc where
    getLocalData :: m (SscLocalData ssc)
    setLocalData :: SscLocalData ssc -> m ()
    modifyLocalData :: ((SscGlobalState ssc, SscLocalData ssc)
                     -> (a, SscLocalData ssc)) -> m a

    default getLocalData :: (MonadTrans t, MonadSscLD ssc m', t m' ~ m) => t m' (SscLocalData ssc)
    getLocalData = lift getLocalData

    default setLocalData :: (MonadTrans t, MonadSscLD ssc m', t m' ~ m) => SscLocalData ssc -> t m' ()
    setLocalData = lift . setLocalData

    default modifyLocalData :: (MonadTrans t, MonadSscLD ssc m', t m' ~ m) => ((SscGlobalState ssc, SscLocalData ssc)
                     -> (a, SscLocalData ssc)) -> t m' a
    modifyLocalData = lift . modifyLocalData

instance (Monad m, MonadSscLD ssc m) => MonadSscLD ssc (ReaderT x m)
instance (Monad m, MonadSscLD ssc m) => MonadSscLD ssc (DHTResponseT s m)
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
       (MonadSscLD ssc m, SscLocalDataClass ssc)
    => SlotId -> m (SscPayload ssc)
sscGetLocalPayload = sscRunLocalQuery . sscGetLocalPayloadQ @ssc

sscApplyGlobalState
    :: forall ssc m.
       (MonadSscLD ssc m, SscLocalDataClass ssc)
    =>  m ()
sscApplyGlobalState =
    modifyLocalData (\(gs, ld) -> runState (sscApplyGlobalStateU @ssc gs) ld)
