{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}

-- | This module adds extra ecapsulation by hiding acid-state.

module Pos.State.State
       ( NodeState
       , MonadDB (getNodeState)
       , WorkModeDB
       , openState
       , openMemState
       , closeState

       -- * Simple getters.
       , getBlock
       , getBlockByDepth
       , getHeadBlock
       , getBestChain
       , getLeaders
       , getLocalTxs
       , isTxVerified
       , getGlobalMpcData
       , mayBlockBeUseful

       -- * Operations with effects.
       , ProcessBlockRes (..)
       , ProcessTxRes (..)
       , createNewBlock
       , processBlock
       , processNewSlot
       , processTx

       -- * Functions for generating seed by SSC algorithm.
       , getThreshold
       , getParticipants

       -- * SscGodTossing simple getters and setters.
       , getOurShares
       ) where

import           Crypto.Random            (seedNew, seedToInteger)
import           Data.Acid                (EventResult, EventState, QueryEvent,
                                           UpdateEvent)
import           Data.Default             (Default)
import           Data.List.NonEmpty       (NonEmpty)
import           Pos.DHT                  (DHTResponseT)
import           Serokell.Util            (VerificationRes)
import           Universum

import           Pos.Crypto               (PublicKey, SecretKey, Share, Threshold,
                                           VssKeyPair, VssPublicKey)
import           Pos.Slotting             (MonadSlots, getCurrentSlot)
import           Pos.Ssc.Class.Storage    (SscStorageClass (..), SscStorageMode)
import           Pos.Ssc.Class.Types      (Ssc (SscGlobalState, SscPayload, SscStorage))
import           Pos.State.Acidic         (DiskState, tidyState)
import qualified Pos.State.Acidic         as A
import           Pos.State.Storage        (ProcessBlockRes (..), ProcessTxRes (..),
                                           Storage)
import           Pos.Statistics.StatEntry ()
import           Pos.Types                (Block, EpochIndex, GenesisBlock, HeaderHash,
                                           MainBlock, MainBlockHeader, SlotId,
                                           SlotLeaders, Tx)

-- | NodeState encapsulates all the state stored by node.
class Monad m => MonadDB ssc m | m -> ssc where
    getNodeState :: m (NodeState ssc)

-- | Convenient type class to avoid passing NodeState throughout the code.
instance (Monad m, MonadDB ssc m) => MonadDB ssc (ReaderT r m) where
    getNodeState = lift getNodeState

instance (MonadDB ssc m, Monad m) => MonadDB ssc (StateT s m) where
    getNodeState = lift getNodeState

instance (Monad m, MonadDB ssc m) => MonadDB ssc (DHTResponseT m) where
    getNodeState = lift getNodeState

-- | IO monad with db access.
type WorkModeDB ssc m = (MonadIO m, MonadDB ssc m)

-- | State of the node.
type NodeState ssc = DiskState ssc

type QUConstraint ssc m = (SscStorageMode ssc, WorkModeDB ssc m)

-- | Open NodeState, reading existing state from disk (if any).
openState
    :: (SscStorageMode ssc, Default (SscStorage ssc),
        MonadIO m, MonadSlots m)
    => Maybe (Storage ssc)
    -> Bool
    -> FilePath
    -> m (NodeState ssc)
openState storage deleteIfExists fp =
    openStateDo $ maybe (A.openState deleteIfExists fp)
                        (\s -> A.openStateCustom s deleteIfExists fp)
                        storage


-- | Open NodeState which doesn't store anything on disk. Everything
-- is stored in memory and will be lost after shutdown.
openMemState
    :: (SscStorageMode ssc, Default (SscStorage ssc),
        MonadIO m, MonadSlots m)
    => Maybe (Storage ssc)
    -> m (NodeState ssc)
openMemState = openStateDo . maybe A.openMemState A.openMemStateCustom

openStateDo :: (MonadIO m, MonadSlots m, SscStorageMode ssc)
            => m (DiskState ssc)
            -> m (NodeState ssc)
openStateDo openDiskState = do
    st <- openDiskState
    _ <- A.update st . A.ProcessNewSlot =<< getCurrentSlot
    st <$ tidyState st

-- | Safely close NodeState.
closeState :: (MonadIO m, SscStorageClass ssc) => NodeState ssc -> m ()
closeState = A.closeState

queryDisk
    :: (SscStorageClass ssc,
        EventState event ~ (Storage ssc),
        QueryEvent event, WorkModeDB ssc m)
    => event
    -> m (EventResult event)
queryDisk e = flip A.query e =<< getNodeState

updateDisk
    :: (SscStorageClass ssc,
        EventState event ~ (Storage ssc),
        UpdateEvent event, WorkModeDB ssc m)
    => event
    -> m (EventResult event)
updateDisk e = flip A.update e =<< getNodeState

-- | Get list of slot leaders for the given epoch. Empty list is returned
-- if no information is available.
getLeaders :: QUConstraint ssc m => EpochIndex -> m (Maybe SlotLeaders)
getLeaders = queryDisk . A.GetLeaders

-- | Get Block by hash.
getBlock :: QUConstraint ssc m => HeaderHash ssc -> m (Maybe (Block ssc))
getBlock = queryDisk . A.GetBlock

-- | Get Block by depth
getBlockByDepth :: QUConstraint ssc m => Word -> m (Maybe (Block ssc))
getBlockByDepth = queryDisk . A.GetBlockByDepth

-- | Get block which is the head of the __best chain__.
getHeadBlock :: QUConstraint ssc m => m (Block ssc)
getHeadBlock = queryDisk A.GetHeadBlock

-- | Return current best chain.
getBestChain :: QUConstraint ssc m => m (NonEmpty (Block ssc))
getBestChain = queryDisk A.GetBestChain

-- | Get local transactions list.
getLocalTxs :: QUConstraint ssc m => m (HashSet Tx)
getLocalTxs = queryDisk A.GetLocalTxs

-- | Checks if tx is verified
isTxVerified :: QUConstraint ssc m => Tx -> m Bool
isTxVerified = queryDisk . A.IsTxVerified

-- | Get global SSC data.
getGlobalMpcData :: QUConstraint ssc m => m (SscGlobalState ssc)
getGlobalMpcData = queryDisk A.GetGlobalSscPayload

-- | Check that block header is correct and claims to represent block
-- which may become part of blockchain.
mayBlockBeUseful :: QUConstraint ssc m => SlotId -> MainBlockHeader ssc -> m VerificationRes
mayBlockBeUseful si = queryDisk . A.MayBlockBeUseful si

-- | Create new block on top of currently known best chain, assuming
-- we are slot leader.
createNewBlock :: QUConstraint ssc m
               => SecretKey
               -> SlotId
               -> SscPayload ssc
               -> m (Maybe (MainBlock ssc))
createNewBlock sk si = updateDisk . A.CreateNewBlock sk si

-- | Process transaction received from other party.
processTx :: QUConstraint ssc m => Tx -> m ProcessTxRes
processTx = updateDisk . A.ProcessTx

-- | Notify NodeState about beginning of new slot. Ideally it should
-- be used before all other updates within this slot.
processNewSlot :: QUConstraint ssc m => SlotId -> m (Maybe (GenesisBlock ssc))
processNewSlot = updateDisk . A.ProcessNewSlot

-- | Process some Block received from the network.
processBlock :: QUConstraint ssc m
             => SlotId
             -> Block ssc
             -> m (ProcessBlockRes ssc)
processBlock si = updateDisk . A.ProcessBlock si

-- | Functions for generating seed by SSC algorithm
getParticipants
    :: QUConstraint ssc m
    => EpochIndex
    -> m (Maybe (NonEmpty VssPublicKey))
getParticipants = queryDisk . A.GetParticipants

-- | Figure out the threshold (i.e. how many secret shares would be required
-- to recover each node's secret).
getThreshold :: QUConstraint ssc m
             => EpochIndex
             -> m (Maybe Threshold)
getThreshold = queryDisk . A.GetThreshold

----------------------------------------------------------------------------
-- Related to SscGodTossing
----------------------------------------------------------------------------

-- | Decrypt shares (in commitments) that are intended for us and that we can
-- decrypt.
getOurShares
    :: QUConstraint ssc m
    => VssKeyPair -> m (HashMap PublicKey Share)
getOurShares ourKey = do
    randSeed <- liftIO seedNew
    queryDisk $ A.GetOurShares ourKey (seedToInteger randSeed)
