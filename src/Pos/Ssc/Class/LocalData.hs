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
       , sscApplyGlobalPayload
       , sscProcessNewSlot
       ) where

import           Control.Lens            (Lens')
import           Universum

import           Pos.Ssc.Class.Types     (Ssc (..))
import           Pos.State.Storage.Types (ProcessBlockRes)
import           Pos.Types.Types         (Block, SlotId)

type LocalQuery ssc a = forall m . ( HasSscLocalData ssc (SscLocalData ssc)
                                   , MonadReader (SscLocalData ssc) m
                                   ) => m a
type LocalUpdate ssc a = forall m . ( HasSscLocalData ssc (SscLocalData ssc)
                                    , MonadState (SscLocalData ssc) m
                                    ) => m a
class HasSscLocalData ssc a where
    sscLocalData :: Lens' a (SscLocalData ssc)

instance (SscLocalData ssc ~ a) => HasSscLocalData ssc a where
    sscLocalData = identity

class Monad m => MonadSscLD ssc m | m -> ssc where
    getLocalData :: m (SscLocalData ssc)
    setLocalData :: SscLocalData ssc -> m ()

-- | This type class abstracts local data used for SSC. Local means
-- that it is not stored in blocks.
class Ssc ssc => SscLocalDataClass ssc where
    sscEmptyLocalData :: SscLocalData ssc
    sscGetLocalPayloadQ :: SlotId -> LocalQuery ssc (SscPayload ssc)
    sscApplyGlobalPayloadU :: ProcessBlockRes ssc -> SscPayload ssc -> LocalUpdate ssc ()
    sscProcessNewSlotU :: SlotId -> LocalUpdate ssc ()

sscRunLocalQuery
    :: forall ssc m a.
       MonadSscLD ssc m
    => Reader (SscLocalData ssc) a -> m a
sscRunLocalQuery query = runReader query <$> getLocalData @ssc

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

sscApplyGlobalPayload
    :: forall ssc m.
       (MonadSscLD ssc m, SscLocalDataClass ssc)
    => ProcessBlockRes ssc -> SscPayload ssc -> m ()
sscApplyGlobalPayload res = sscRunLocalUpdate . sscApplyGlobalPayloadU @ssc res

sscProcessNewSlot
    :: forall ssc m.
       (MonadSscLD ssc m, SscLocalDataClass ssc)
    => SlotId -> m ()
sscProcessNewSlot = sscRunLocalUpdate . sscProcessNewSlotU @ssc
