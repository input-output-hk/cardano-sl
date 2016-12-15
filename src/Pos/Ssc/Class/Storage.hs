{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}

-- | Storage for generic Shared Seed calculation implementation.

module Pos.Ssc.Class.Storage
       ( SscStorageClass(..)
       , HasSscStorage(..)
       , MonadSscGS (..)

       , SscUpdate
       , SscQuery
       , SscStorageMode
       ) where

import           Control.Lens            (Lens')
import           Control.Monad.Trans     (MonadTrans)
import           Data.List.NonEmpty      (NonEmpty)
import           Data.SafeCopy           (SafeCopy)
import           Serokell.Util.Verify    (VerificationRes)
import           Universum

import           Pos.Crypto              (EncShare, Threshold, VssPublicKey)
import           Pos.Ssc.Class.Types     (Ssc (..))
import           Pos.State.Storage.Types (AltChain)
import           Pos.Types.Types         (Address, EpochIndex, SlotLeaders, Utxo)
import           Pos.Util                (AsBinary)

-- | Generic @SSC@ query.
type SscUpdate ssc a =
    forall m x. (HasSscStorage ssc x, MonadState x m) => m a

-- | Generic @SSC@ update.
--
-- If this type ever changes to include side effects (error reporting, etc)
-- we might have to change 'mpcVerifyBlock' because currently it works by
-- simulating block application and we don't want block verification to have
-- any side effects. The compiler will warn us if it happens, though.
-- | Monad reader on something containing `SscStorage` inside.
type SscQuery ssc a =
    forall m x. (HasSscStorage ssc x, MonadReader x m) => m a

class Monad m => MonadSscGS ssc m | m -> ssc where
    getGlobalState     :: m (SscGlobalState ssc)
    setGlobalState    :: SscLocalData ssc -> m ()
    modifyGlobalState :: (SscGlobalState ssc -> (a, SscGlobalState ssc)) -> m a

    default getGlobalState :: MonadTrans t => t m (SscGlobalState ssc)
    getGlobalState = lift getGlobalState

    default setGlobalState :: MonadTrans t => SscLocalData ssc -> t m ()
    setGlobalState = lift . setGlobalState

    default modifyGlobalState :: MonadTrans t =>
                                 (SscGlobalState ssc -> (a, SscGlobalState ssc)) -> t m a
    modifyGlobalState = lift . modifyGlobalState

-- | Class of objects that we can retrieve 'SscStorage' from.
class HasSscStorage ssc a where
    sscStorage :: Lens' a (SscStorage ssc)

-- | Class for @SSC@ storage.
class Ssc ssc => SscStorageClass ssc where
    -- sscCalculateSeed :: SscQuery ssc (Either (SscSeedError ssc) SharedSeed)

    sscApplyBlocks :: AltChain ssc -> SscUpdate ssc ()

    -- | Rollback application of last 'n' blocks.  blocks. If there
    -- are less blocks than 'n' is, just leaves an empty ('def')
    -- version.
    sscRollback :: Word -> SscUpdate ssc ()

    -- | Get global SSC data.
    sscGetGlobalState :: SscQuery ssc (SscGlobalState ssc)
    -- | Get global SSC data for the state that was observed N blocks ago.
    sscGetGlobalStateByDepth :: Word -> SscQuery ssc (Maybe (SscGlobalState ssc))
    -- | Verify Ssc-related predicates of block sequence which is
    -- about to be applied. It should check that SSC payload will be
    -- consistent if this blocks are applied (after possible rollback
    -- if first argument isn't zero).
    sscVerifyBlocks :: Word -> AltChain ssc -> SscQuery ssc VerificationRes

    -- [CSL-103]: these 3 functions should be replaced with something different.
    sscGetOurShares
        :: (AsBinary VssPublicKey)
        -> SscQuery ssc (HashMap Address (AsBinary EncShare))
    sscGetParticipants :: Word -> Utxo ->
                          SscQuery ssc (Maybe (NonEmpty (AsBinary VssPublicKey)))
    sscCalculateLeaders :: EpochIndex -> Utxo -> Threshold ->
                           SscQuery ssc (Either (SscSeedError ssc)  SlotLeaders)

-- | Type constraint for actions to operate withing @SSC@ storage.
type SscStorageMode ssc = (SscStorageClass ssc, SafeCopy ssc)
