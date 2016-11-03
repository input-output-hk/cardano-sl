{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}

module Pos.Ssc.Class.Storage
       ( SscStorageClass(..)
       , HasSscStorage(..)

       , SscUpdate
       , SscQuery
       ) where

import           Control.Lens            (Lens')
import           Universum

import           Pos.Ssc.Class.Types     (SscTypes (..))
import           Pos.State.Storage.Types (AltChain)
import           Pos.Types.Types         (FtsSeed, SlotId)

type SscUpdate ssc a =
    forall m x. (HasSscStorage ssc x, MonadState x m) => m a

-- If this type ever changes to include side effects (error reporting, etc)
-- we might have to change 'mpcVerifyBlock' because currently it works by
-- simulating block application and we don't want block verification to have
-- any side effects. The compiler will warn us if it happens, though.
type SscQuery ssc a =
    forall m x. (HasSscStorage ssc x, MonadReader x m) => m a

class HasSscStorage ssc a where
    sscStorage :: Lens' a (SscStorage ssc)

class SscTypes ssc => SscStorageClass ssc where
    -- sscCalculateSeed :: SscQuery ssc (Either (SscSeedError ssc) FtsSeed)

    sscApplyBlocks :: AltChain ssc -> SscUpdate ssc ()
    -- | Should be executed before doing any updates within given slot.
    sscPrepareToNewSlot :: SlotId -> SscUpdate ssc ()
    -- | Do something with given message, result is whether message
    -- has been processed successfully (implementation defined).
    sscProcessMessage :: SscMessage ssc -> SscUpdate ssc Bool

    -- TODO: move the rest of methods here
