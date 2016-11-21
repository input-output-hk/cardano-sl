{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}

-- | This module defines type classes for local data storage as well
-- as convenient wrappers.

module Pos.Ssc.Class.LocalData
       ( Query
       , Update
       , SscLocalDataClass (..)
       , HasSscLocalData (..)
       , MonadSscLD (..)
       , sscGetLocalPayload
       ) where

import           Control.Lens        (Lens')
import           Universum

import           Pos.Ssc.Class.Types (Ssc (..))
import           Pos.Types.Types     (SlotId)

type Query ssc a = forall m. ( HasSscLocalData ssc (SscLocalData ssc)
                             , MonadReader (SscLocalData ssc) m
                             ) => m a
type Update ssc a = forall m. ( HasSscLocalData ssc (SscLocalData ssc)
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
    -- maybe should take global payload too
    sscGetLocalPayloadQ :: SlotId -> Query ssc (SscPayload ssc)

sscGetLocalPayload
    :: forall ssc m.
       (MonadSscLD ssc m, SscLocalDataClass ssc)
    => SlotId -> m (SscPayload ssc)
sscGetLocalPayload slotId =
    runReader (sscGetLocalPayloadQ @ssc slotId) <$> getLocalData @ssc
