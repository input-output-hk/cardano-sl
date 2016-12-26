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

-- | This module defines type class for local data storage.

module Pos.Ssc.Class.LocalData
       ( LocalQuery
       , LocalUpdate
       , SscLocalDataClass (..)
       ) where

import           Universum

import           Pos.Ssc.Class.Types (Ssc (..))
import           Pos.Types.Types     (SlotId)

----------------------------------------------------------------------------
-- Modern
----------------------------------------------------------------------------

type LocalQuery ssc a =  forall m . (MonadReader (SscLocalData ssc) m) => m a
type LocalUpdate ssc a = forall m . (MonadState (SscLocalData ssc) m) => m a

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
