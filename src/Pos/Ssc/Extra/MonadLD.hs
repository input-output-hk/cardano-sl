{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DefaultSignatures      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}

-- | Type class to work with SscLocalData.

module Pos.Ssc.Extra.MonadLD
       ( MonadSscLDM (..)
       , sscApplyGlobalStateM
       , sscGetLocalPayloadM
       , sscRunLocalQueryM
       , sscRunLocalUpdateM
       -- * Old
       , MonadSscLD (..)
       , sscRunLocalQuery
       , sscRunLocalUpdate
       , sscGetLocalPayload
       , sscApplyGlobalState
       ) where

import           Control.Monad.Trans     (MonadTrans)
import           Universum

import           Pos.DHT.Model           (DHTResponseT)
import           Pos.DHT.Real            (KademliaDHT)
import           Pos.Ssc.Class.LocalData (SscLocalDataClass (..))
import           Pos.Ssc.Class.Types     (Ssc (..))
import           Pos.Types.Types         (SlotId)

class Monad m => MonadSscLDM ssc m | m -> ssc where
    getLocalDataM :: m (SscLocalData ssc)
    setLocalDataM :: SscLocalData ssc -> m ()
    modifyLocalDataM :: ((SscGlobalStateM ssc, SscLocalData ssc)
                     -> (a, SscLocalData ssc)) -> m a

    default getLocalDataM :: MonadTrans t => t m (SscLocalData ssc)
    getLocalDataM = lift getLocalDataM

    default setLocalDataM :: MonadTrans t => SscLocalData ssc -> t m ()
    setLocalDataM = lift . setLocalDataM

    default modifyLocalDataM :: MonadTrans t => ((SscGlobalStateM ssc, SscLocalData ssc)
                     -> (a, SscLocalData ssc)) -> t m a
    modifyLocalDataM = lift . modifyLocalDataM

instance (Monad m, MonadSscLDM ssc m) => MonadSscLDM ssc (ReaderT x m)
instance (Monad m, MonadSscLDM ssc m) => MonadSscLDM ssc (DHTResponseT s m)
instance (MonadSscLDM ssc m, Monad m) => MonadSscLDM ssc (KademliaDHT m)

-- | Convenient wrapper to run LocalQuery in MonadSscLD.
sscRunLocalQueryM
    :: forall ssc m a.
       MonadSscLDM ssc m
    => Reader (SscLocalData ssc) a -> m a
sscRunLocalQueryM query = runReader query <$> getLocalDataM @ssc

-- | Convenient wrapper to run LocalUpdate in MonadSscLD.
sscRunLocalUpdateM
    :: MonadSscLDM ssc m
    => State (SscLocalData ssc) a -> m a
sscRunLocalUpdateM upd =
    modifyLocalDataM (\(_, l) -> runState upd l)

----------------------------------------------------------------------------
-- Methods for using in MonadSscLD
----------------------------------------------------------------------------

sscGetLocalPayloadM
    :: forall ssc m.
       (MonadSscLDM ssc m, SscLocalDataClass ssc)
    => SlotId -> m (SscPayload ssc)
sscGetLocalPayloadM = sscRunLocalQueryM . sscGetLocalPayloadQ @ssc

sscApplyGlobalStateM
    :: forall ssc m.
       (MonadSscLDM ssc m, SscLocalDataClass ssc)
    =>  SscGlobalStateM ssc -> m ()
sscApplyGlobalStateM = notImplemented --sscRunLocalUpdateM . sscApplyGlobalStateU @ssc

-- | Monad which has read-write access to LocalData.
class Monad m => MonadSscLD ssc m | m -> ssc where
    getLocalData :: m (SscLocalData ssc)
    setLocalData :: SscLocalData ssc -> m ()

    default getLocalData :: MonadTrans t => t m (SscLocalData ssc)
    getLocalData = lift getLocalData

    default setLocalData :: MonadTrans t => SscLocalData ssc -> t m ()
    setLocalData = lift . setLocalData

instance (Monad m, MonadSscLD ssc m) => MonadSscLD ssc (ReaderT x m)
instance (Monad m, MonadSscLD ssc m) => MonadSscLD ssc (DHTResponseT s m)
instance (MonadSscLD ssc m, Monad m) => MonadSscLD ssc (KademliaDHT m)

----------------------------------------------------------------------------
-- Helpers for transform from MonadSscLD to Reader/State monad and back
----------------------------------------------------------------------------

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
sscRunLocalUpdate upd = do
    (res, newLocalData) <- runState upd <$> getLocalData
    res <$ setLocalData newLocalData
----------------------------------------------------------------------------
-- Methods for using in MonadSscLD
----------------------------------------------------------------------------
sscGetLocalPayload
    :: forall ssc m.
       (MonadSscLD ssc m, SscLocalDataClass ssc)
    => SlotId -> m (SscPayload ssc)
sscGetLocalPayload = sscRunLocalQuery . sscGetLocalPayloadQ @ssc

sscApplyGlobalState
    :: forall ssc m.
       (MonadSscLD ssc m, SscLocalDataClass ssc)
    =>  SscGlobalState ssc -> m ()
sscApplyGlobalState = notImplemented --sscRunLocalUpdate . sscApplyGlobalStateU @ssc
