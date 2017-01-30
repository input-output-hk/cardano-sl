{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Storage for generic Shared Seed calculation implementation.

module Pos.Ssc.Class.Storage
       (
         -- * Modern
         SscStorageClass (..)
       , SscGlobalQuery
       , SscGlobalUpdate
       ) where

import           System.Wlog         (WithLogger)
import           Universum

import           Pos.DB.Class        (MonadDB)
import           Pos.Lrc.Types       (Richmen)
import           Pos.Ssc.Class.Types (Ssc (..))
import           Pos.Types           (Block, EpochIndex, SharedSeed)
import           Pos.Util            (NE, NewestFirst, OldestFirst)

----------------------------------------------------------------------------
-- Modern
----------------------------------------------------------------------------

type SscGlobalQuery ssc a =  forall m . (MonadReader (SscGlobalState ssc) m, WithLogger m) => m a
type SscGlobalUpdate ssc a = forall m . (MonadState (SscGlobalState ssc) m, WithLogger m) => m a

class Ssc ssc => SscStorageClass ssc where
    sscLoadGlobalState
        :: (MonadDB ssc m, WithLogger m)
        => m (SscGlobalState ssc)

    sscApplyBlocksM
        :: OldestFirst NE (Block ssc) -> SscGlobalUpdate ssc ()

    -- | Rollback application of blocks.
    sscRollbackU
        :: NewestFirst NE (Block ssc) -> SscGlobalUpdate ssc ()

    -- | Verify Ssc-related predicates of block sequence which is
    -- about to be applied. It should check that SSC payload will be
    -- consistent if this blocks are applied (after possible rollback
    -- if first argument isn't zero).
    sscVerifyBlocksM
        :: Bool
        -> Richmen
        -> OldestFirst NE (Block ssc)
        -> SscGlobalQuery ssc (Either (SscVerifyError ssc) ())

    sscCalculateSeedQ
        :: EpochIndex
        -> SscGlobalQuery ssc (Either (SscSeedError ssc) SharedSeed)
