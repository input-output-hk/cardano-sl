{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE CPP                    #-}
{-# LANGUAGE DefaultSignatures      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}

-- | This module defines type class for local data storage.

module Pos.Ssc.Class.LocalData
       ( LocalQuery
       , LocalUpdate
       , SscLocalDataClass (..)
       , HasSscLocalData (..)
       ) where

import           Control.Lens        (Lens')
import           Universum

import           Pos.Ssc.Class.Types (Ssc (..))
import           Pos.Types.Types     (SlotId)

----------------------------------------------------------------------------
-- Modern
----------------------------------------------------------------------------

#ifdef MODERN
type LocalQuery ssc a = forall m . (MonadReader (SscLocalData ssc) m) => m a
type LocalUpdate ssc a = forall m .(MonadState (SscLocalData ssc) m) => m a
#else
type LocalQuery ssc a = forall m . ( HasSscLocalData ssc (SscLocalData ssc)
                                   , MonadReader (SscLocalData ssc) m
                                   ) => m a
type LocalUpdate ssc a = forall m . ( HasSscLocalData ssc (SscLocalData ssc)
                                    , MonadState (SscLocalData ssc) m
                                    ) => m a
#endif

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
#ifdef MODERN
    sscApplyGlobalStateU :: SscGlobalState ssc -> LocalUpdate ssc ()
#else
    sscApplyGlobalStateU :: SscGlobalState ssc -> LocalUpdate ssc ()
#endif
----------------------------------------------------------------------------
-- LEGACY
----------------------------------------------------------------------------

-- | Type class which allows usage of classy pattern.
class HasSscLocalData ssc a where
    sscLocalData :: Lens' a (SscLocalData ssc)

instance (SscLocalData ssc ~ a) => HasSscLocalData ssc a where
    sscLocalData = identity
