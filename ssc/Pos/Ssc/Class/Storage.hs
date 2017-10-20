{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes          #-}

-- | Global state of generic Shared Seed Calculation implementation.

module Pos.Ssc.Class.Storage
       ( SscGStateClass (..)
       , SscGlobalQuery
       , SscGlobalUpdate
       , SscVerifier
       ) where

import           Control.Monad.Except (MonadError)
import qualified Crypto.Random        as Rand
import           System.Wlog          (WithLogger)
import           Universum

import           Pos.Core             (BlockVersionData, EpochIndex, SharedSeed)
import           Pos.DB               (MonadDBRead, SomeBatchOp)
import           Pos.Lrc.Types        (RichmenStakes)
import           Pos.Ssc.Class.Types  (Ssc (..), SscBlock)
import           Pos.Ssc.SeedError    (SscSeedError)
import           Pos.Util.Chrono      (NE, NewestFirst, OldestFirst)

----------------------------------------------------------------------------
-- Modern
----------------------------------------------------------------------------

type SscGlobalQuery a =
    forall m . (MonadReader SscGlobalState m, WithLogger m) => m a

type SscGlobalUpdate a =
    forall m . (MonadState SscGlobalState m, WithLogger m, Rand.MonadRandom m) => m a

type SscVerifyMode m =
    ( MonadState SscGlobalState m
    , WithLogger m
    , MonadError SscVerifyError m
    , Rand.MonadRandom m
    )

type SscVerifier a = forall m . SscVerifyMode m => m a

class Ssc => SscGStateClass where
    -- | Load global state from DB by recreating it from recent blocks.
    sscLoadGlobalState
        :: (MonadDBRead m, WithLogger m)
        => m SscGlobalState
    -- | Dump global state to DB.
    sscGlobalStateToBatch :: SscGlobalState -> [SomeBatchOp]
    -- | Rollback application of blocks.
    sscRollbackU :: NewestFirst NE SscBlock -> SscGlobalUpdate ()
    -- | Verify SSC-related part of given blocks with respect to
    -- current GState and apply them on success.
    -- Blocks must be from the same epoch.
    sscVerifyAndApplyBlocks
        :: RichmenStakes
        -> BlockVersionData
        -> OldestFirst NE SscBlock
        -> SscVerifier ()
    -- | Calculate 'SharedSeed' for given epoch using 'SscGlobalState'.
    sscCalculateSeedQ
        :: EpochIndex
        -> RichmenStakes
        -> SscGlobalQuery (Either SscSeedError SharedSeed)
