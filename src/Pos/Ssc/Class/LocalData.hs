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

-- | This module defines type classes for local data storage as well
-- as convenient wrappers.

module Pos.Ssc.Class.LocalData
       ( LocalQuery
       , LocalUpdate
       , SscLocalDataClass (..)
       , HasSscLocalData (..)
       , MonadSscLD (..)

       , sscRunLocalQuery
       , sscRunLocalUpdate
       , sscGetLocalPayload
       , sscApplyGlobalState
       ) where

import           Control.Lens        (Lens')
import           Control.Monad.Trans (MonadTrans)

import           Pos.DHT.Model       (DHTResponseT)
import           Pos.DHT.Real        (KademliaDHT)
import           Universum

import           Pos.Ssc.Class.Types (Ssc (..))
import           Pos.Types.Types     (SlotId)

type LocalQuery ssc a = forall m . ( HasSscLocalData ssc (SscLocalData ssc)
                                   , MonadReader (SscLocalData ssc) m
                                   ) => m a
type LocalUpdate ssc a = forall m . ( HasSscLocalData ssc (SscLocalData ssc)
                                    , MonadState (SscLocalData ssc) m
                                    ) => m a

-- | Type class which allows usage of classy pattern.
class HasSscLocalData ssc a where
    sscLocalData :: Lens' a (SscLocalData ssc)

instance (SscLocalData ssc ~ a) => HasSscLocalData ssc a where
    sscLocalData = identity

-- | Monad which has read-write access to LocalData.
class Monad m => MonadSscLD ssc m | m -> ssc where
    getLocalData :: m (SscLocalData ssc)
    setLocalData :: SscLocalData ssc -> m ()
    modifyLocalData :: ((SscGlobalState ssc, SscLocalData ssc)
                     -> (a, SscLocalData ssc)) -> m a

    default getLocalData :: MonadTrans t => t m (SscLocalData ssc)
    getLocalData = lift getLocalData

    default setLocalData :: MonadTrans t => SscLocalData ssc -> t m ()
    setLocalData = lift . setLocalData

    default modifyLocalData :: MonadTrans t => ((SscGlobalState ssc, SscLocalData ssc)
                     -> (a, SscLocalData ssc)) -> t m a
    modifyLocalData = lift . modifyLocalData

instance (Monad m, MonadSscLD ssc m) => MonadSscLD ssc (ReaderT x m)
instance (Monad m, MonadSscLD ssc m) => MonadSscLD ssc (DHTResponseT s m)
instance (MonadSscLD ssc m, Monad m) => MonadSscLD ssc (KademliaDHT m)

-- | This type class abstracts local data used for SSC. Local means
-- that it is not stored in blocks.
class Ssc ssc => SscLocalDataClass ssc where
    -- | Empty local data which is created on start.
    sscEmptyLocalData :: SscLocalData ssc
    -- | Get local payload to be put into main block corresponding to
    -- given SlotId.
    sscGetLocalPayloadQ :: SlotId -> LocalQuery ssc (SscPayload ssc)
    -- | Update LocalData using global data from blocks (last version
    -- of best known chain).
    sscApplyGlobalStateU :: SscGlobalState ssc -> LocalUpdate ssc ()

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
sscRunLocalUpdate upd =
    modifyLocalData (\(_, l) -> runState upd l)

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
sscApplyGlobalState = sscRunLocalUpdate . sscApplyGlobalStateU @ssc
