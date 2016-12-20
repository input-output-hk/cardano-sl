{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DefaultSignatures      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}

-- | Storage for generic Shared Seed calculation implementation.

module Pos.Ssc.Class.Storage
       (
         -- * Modern
         HasSscStorage(..)
       , SscStorageClassM (..)
       , MonadSscGS (..)

       , SscGlobalQueryM
       , SscGlobalUpdateM
       , sscRunGlobalQuery
       , sscRunGlobalModify

         -- * Old
       , SscUpdate
       , SscQuery
       , SscStorageClass(..)
       , SscStorageMode
       ) where

import           Control.Lens            (Lens')
import           Control.Monad.Except    (ExceptT)
import           Control.Monad.Trans     (MonadTrans)
import           Control.TimeWarp.Rpc    (ResponseT)
import           Data.List.NonEmpty      (NonEmpty)
import           Data.SafeCopy           (SafeCopy)
import           Serokell.Util.Verify    (VerificationRes)
import           Universum

import           Pos.Crypto              (EncShare, Threshold, VssPublicKey)
import           Pos.DHT.Model.Class     (DHTResponseT)
import           Pos.DHT.Real            (KademliaDHT)
import           Pos.Ssc.Class.Types     (Ssc (..))
import           Pos.State.Storage.Types (AltChain)
import           Pos.Types.Types         (Address, EpochIndex, HeaderHash, SlotLeaders,
                                          Utxo)
import           Pos.Util                (AsBinary)

----------------------------------------------------------------------------
-- Modern
----------------------------------------------------------------------------

type SscGlobalQueryM ssc a =  forall m . (MonadReader (SscGlobalStateM ssc) m) => m a
type SscGlobalUpdateM ssc a = forall m . (MonadState (SscGlobalStateM ssc) m) => m a

class Monad m => MonadSscGS ssc m | m -> ssc where
    getGlobalState    :: m (SscGlobalStateM ssc)
    setGlobalState    :: SscGlobalStateM ssc -> m ()
    modifyGlobalState :: (SscGlobalStateM ssc -> (a, SscGlobalStateM ssc)) -> m a

    default getGlobalState :: MonadTrans t => t m (SscGlobalStateM ssc)
    getGlobalState = lift getGlobalState

    default setGlobalState :: MonadTrans t => SscGlobalStateM ssc -> t m ()
    setGlobalState = lift . setGlobalState

    default modifyGlobalState :: MonadTrans t =>
                                 (SscGlobalStateM ssc -> (a, SscGlobalStateM ssc)) -> t m a
    modifyGlobalState = lift . modifyGlobalState

instance MonadSscGS ssc m => MonadSscGS ssc (ReaderT a m) where
instance MonadSscGS ssc m => MonadSscGS ssc (ExceptT a m) where
instance MonadSscGS ssc m => MonadSscGS ssc (ResponseT s m) where
instance MonadSscGS ssc m => MonadSscGS ssc (DHTResponseT s m) where
instance MonadSscGS ssc m => MonadSscGS ssc (KademliaDHT m) where

class Monad m => SscStorageClassM ssc m | m -> ssc where
    sscEmptyGlobalState :: m (SscGlobalStateM ssc)
    sscLoadGlobalState :: HeaderHash ssc -> m (SscGlobalStateM ssc)

    -- This must be here. We should remove SscStorageClass, right?
    sscApplyBlocksM :: AltChain ssc -> m ()

    -- | Rollback application of last 'n' blocks.  blocks. If there
    -- are less blocks than 'n' is, just leaves an empty ('def')
    -- version.
    sscRollbackM :: AltChain ssc -> m ()

    -- | Verify Ssc-related predicates of block sequence which is
    -- about to be applied. It should check that SSC payload will be
    -- consistent if this blocks are applied (after possible rollback
    -- if first argument isn't zero).
    sscVerifyBlocksM :: AltChain ssc -> m VerificationRes

    default sscEmptyGlobalState :: MonadTrans t => t m (SscGlobalStateM ssc)
    sscEmptyGlobalState = lift sscEmptyGlobalState

    default sscLoadGlobalState :: MonadTrans t => HeaderHash ssc -> t m (SscGlobalStateM ssc)
    sscLoadGlobalState = lift . sscLoadGlobalState

    default sscApplyBlocksM :: MonadTrans t => AltChain ssc -> t m ()
    sscApplyBlocksM = lift . sscApplyBlocksM

    default sscRollbackM :: MonadTrans t => AltChain ssc -> t m ()
    sscRollbackM = lift . sscRollbackM

    default sscVerifyBlocksM :: MonadTrans t => AltChain ssc -> t m VerificationRes
    sscVerifyBlocksM = lift . sscVerifyBlocksM

instance SscStorageClassM ssc m => SscStorageClassM ssc (ReaderT a m) where
instance SscStorageClassM ssc m => SscStorageClassM ssc (ExceptT a m) where
instance SscStorageClassM ssc m => SscStorageClassM ssc (ResponseT s m) where
instance SscStorageClassM ssc m => SscStorageClassM ssc (DHTResponseT s m) where
instance SscStorageClassM ssc m => SscStorageClassM ssc (KademliaDHT m) where

sscRunGlobalQuery
    :: forall ssc m a.
       MonadSscGS ssc m
    => Reader (SscGlobalStateM ssc) a -> m a
sscRunGlobalQuery query = runReader query <$> getGlobalState @ssc

sscRunGlobalModify
    :: forall ssc m a .
    MonadSscGS ssc m
    => State (SscGlobalStateM ssc) a -> m a
sscRunGlobalModify upd = modifyGlobalState $ runState upd

----------------------------------------------------------------------------
-- Old
----------------------------------------------------------------------------

-- | Generic @SSC@ update.
--
-- If this type ever changes to include side effects (error reporting, etc)
-- we might have to change 'mpcVerifyBlock' because currently it works by
-- simulating block application and we don't want block verification to have
-- any side effects. The compiler will warn us if it happens, though.
-- | Monad reader on something containing `SscStorage` inside.
type SscUpdate ssc a =
    forall m x. (HasSscStorage ssc x, MonadState x m) => m a

-- | Generic @SSC@ query.
type SscQuery ssc a =
    forall m x. (HasSscStorage ssc x, MonadReader x m) => m a

-- | Class of objects that we can retrieve 'SscStorage' from.
class HasSscStorage ssc a where
    sscStorage :: Lens' a (SscStorage ssc)

-- | Class for @SSC@ storage.
class Ssc ssc => SscStorageClass ssc where
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
