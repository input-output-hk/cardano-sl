{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes          #-}

-- | Global state of generic Shared Seed Calculation implementation.

module Pos.Ssc.Class.Storage
       ( SscGStateClass (..)
       , SscGlobalQuery
       , SscGlobalUpdate
       , SscVerifier
       ) where

import           Universum

import           Control.Monad.Except (MonadError)
import           Data.Tagged          (Tagged)
import           System.Wlog          (WithLogger)

import           Pos.Core             (BlockVersionData, EpochIndex, HasCoreConstants,
                                       SharedSeed)
import           Pos.DB               (MonadDBRead, SomeBatchOp)
import           Pos.Lrc.Types        (RichmenStakes)
import           Pos.Ssc.Class.Types  (Ssc (..), SscBlock)
import           Pos.Util.Chrono      (NE, NewestFirst, OldestFirst)

----------------------------------------------------------------------------
-- Modern
----------------------------------------------------------------------------

type SscGlobalQuery ssc a =
    forall m . (MonadReader (SscGlobalState ssc) m, WithLogger m) => m a

type SscGlobalUpdate ssc a
     = forall ctx m. ( MonadState (SscGlobalState ssc) m
                     , WithLogger m
                     , MonadReader ctx m
                     , HasCoreConstants ctx
                     ) => m a

type SscVerifyMode ssc ctx m =
    ( MonadState (SscGlobalState ssc) m
    , WithLogger m
    , MonadError (SscVerifyError ssc) m
    , MonadReader ctx m
    , HasCoreConstants ctx
    )

type SscVerifier ssc a = forall ctx m . SscVerifyMode ssc ctx m => m a

class Ssc ssc => SscGStateClass ssc where
    -- | Load global state from DB by recreating it from recent blocks.
    sscLoadGlobalState
        :: (MonadDBRead m, WithLogger m)
        => m (SscGlobalState ssc)
    -- | Dump global state to DB.
    sscGlobalStateToBatch :: SscGlobalState ssc -> Tagged ssc [SomeBatchOp]
    -- | Rollback application of blocks.
    sscRollbackU :: NewestFirst NE (SscBlock ssc) -> SscGlobalUpdate ssc ()
    -- | Verify SSC-related part of given blocks with respect to
    -- current GState and apply them on success.
    -- Blocks must be from the same epoch.
    sscVerifyAndApplyBlocks
        :: RichmenStakes
        -> BlockVersionData
        -> OldestFirst NE (SscBlock ssc)
        -> SscVerifier ssc ()
    -- | Calculate 'SharedSeed' for given epoch using 'SscGlobalState'.
    sscCalculateSeedQ
        :: EpochIndex
        -> RichmenStakes
        -> SscGlobalQuery ssc (Either (SscSeedError ssc) SharedSeed)
