{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE RankNTypes          #-}

-- | Global state of generic Shared Seed Calculation implementation.

module Pos.Ssc.Class.Storage
       (
         -- * Modern
         SscGStateClass (..)
       , SscGlobalQuery
       , SscGlobalUpdate
       , SscVerifier
       ) where

import           Control.Monad.Except (MonadError)
import           Data.Tagged          (Tagged)
import           System.Wlog          (WithLogger)
import           Universum

import           Pos.DB               (MonadDB, SomeBatchOp)
import           Pos.Lrc.Context      (LrcContext)
import           Pos.Lrc.Types        (RichmenStake)
import           Pos.Ssc.Class.Types  (Ssc (..))
import           Pos.Types            (Block, EpochIndex, SharedSeed)
import           Pos.Util.Chrono      (NE, NewestFirst, OldestFirst)
import           Pos.Util.Context     (HasContext)

----------------------------------------------------------------------------
-- Modern
----------------------------------------------------------------------------

type SscGlobalQuery ssc a =  forall m . (MonadReader (SscGlobalState ssc) m, WithLogger m) => m a
type SscGlobalUpdate ssc a = forall m . (MonadState (SscGlobalState ssc) m, WithLogger m) => m a

type SscVerifyMode ssc m =
    ( MonadState (SscGlobalState ssc) m
    , WithLogger m
    , MonadError (SscVerifyError ssc) m
    )

type SscVerifier ssc a = forall m . SscVerifyMode ssc m => m a

class Ssc ssc =>
      SscGStateClass ssc where
    -- | Load global state from DB by recreating it from recent blocks.
    sscLoadGlobalState
        :: (HasContext LrcContext m, MonadDB m, WithLogger m)
        => m (SscGlobalState ssc)
    -- | Dump global state to DB.
    sscGlobalStateToBatch :: SscGlobalState ssc -> Tagged ssc [SomeBatchOp]
    -- | Rollback application of blocks.
    sscRollbackU :: NewestFirst NE (Block ssc) -> SscGlobalUpdate ssc ()
    -- | Verify SSC-related part of given blocks with respect to
    -- current GState and apply them on success.
    -- Blocks must be from the same epoch.
    sscVerifyAndApplyBlocks
        :: RichmenStake
        -> OldestFirst NE (Block ssc)
        -> SscVerifier ssc ()
    -- | Calculate 'SharedSeed' for given epoch using 'SscGlobalState'.
    sscCalculateSeedQ :: EpochIndex
                      -> SscGlobalQuery ssc (Either (SscSeedError ssc) SharedSeed)
