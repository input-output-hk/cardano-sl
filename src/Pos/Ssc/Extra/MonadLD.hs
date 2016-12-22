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
       ) where

import           Control.Monad.Trans     (MonadTrans)
import           Universum

import           Pos.DHT.Model           (DHTResponseT)
import           Pos.DHT.Real            (KademliaDHT)
import           Pos.Ssc.Class.LocalData (SscLocalDataClassM (..))
import           Pos.Ssc.Class.Types     (Ssc (..))
import           Pos.Types.Types         (SlotId)

class Monad m => MonadSscLDM ssc m | m -> ssc where
    getLocalDataM :: m (SscLocalDataM ssc)
    setLocalDataM :: SscLocalDataM ssc -> m ()
    modifyLocalDataM :: ((SscGlobalStateM ssc, SscLocalDataM ssc)
                     -> (a, SscLocalDataM ssc)) -> m a

    default getLocalDataM :: MonadTrans t => t m (SscLocalDataM ssc)
    getLocalDataM = lift getLocalDataM

    default setLocalDataM :: MonadTrans t => SscLocalDataM ssc -> t m ()
    setLocalDataM = lift . setLocalDataM

    default modifyLocalDataM :: MonadTrans t => ((SscGlobalStateM ssc, SscLocalDataM ssc)
                     -> (a, SscLocalDataM ssc)) -> t m a
    modifyLocalDataM = lift . modifyLocalDataM

instance (Monad m, MonadSscLDM ssc m) => MonadSscLDM ssc (ReaderT x m)
instance (Monad m, MonadSscLDM ssc m) => MonadSscLDM ssc (DHTResponseT s m)
instance (MonadSscLDM ssc m, Monad m) => MonadSscLDM ssc (KademliaDHT m)

-- | Convenient wrapper to run LocalQuery in MonadSscLD.
sscRunLocalQueryM
    :: forall ssc m a.
       MonadSscLDM ssc m
    => Reader (SscLocalDataM ssc) a -> m a
sscRunLocalQueryM query = runReader query <$> getLocalDataM @ssc

-- | Convenient wrapper to run LocalUpdate in MonadSscLD.
sscRunLocalUpdateM
    :: MonadSscLDM ssc m
    => State (SscLocalDataM ssc) a -> m a
sscRunLocalUpdateM upd =
    modifyLocalDataM (\(_, l) -> runState upd l)

----------------------------------------------------------------------------
-- Methods for using in MonadSscLD
----------------------------------------------------------------------------

sscGetLocalPayloadM
    :: forall ssc m.
       (MonadSscLDM ssc m, SscLocalDataClassM ssc)
    => SlotId -> m (SscPayload ssc)
sscGetLocalPayloadM = sscRunLocalQueryM . sscGetLocalPayloadMQ @ssc

sscApplyGlobalStateM
    :: forall ssc m.
       (MonadSscLDM ssc m, SscLocalDataClassM ssc)
    =>  SscGlobalStateM ssc -> m ()
sscApplyGlobalStateM = sscRunLocalUpdateM . sscApplyGlobalStateMU @ssc
