{-# LANGUAGE AllowAmbiguousTypes    #-}
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

instance (Monad m, MonadSscLD ssc m) =>
         MonadSscLD ssc (ReaderT x m) where
    getLocalData = lift getLocalData
    setLocalData = lift . setLocalData

instance (Monad m, MonadSscLD ssc m) =>
         MonadSscLD ssc (DHTResponseT s m) where
    getLocalData = lift getLocalData
    setLocalData = lift . setLocalData

instance (MonadSscLD ssc m, Monad m) =>
         MonadSscLD ssc (KademliaDHT m) where
    getLocalData = lift getLocalData
    setLocalData = lift . setLocalData

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
