{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies    #-}

-- | This module adds extra ecapsulation by hiding acid-state.

module Pos.State.State
       ( NodeState
       , MonadDB (getNodeState)
       , openState
       , openMemState
       , closeState

       -- * Simple getters.
       , getBlock
       , getHeadBlock
       , getLeaders
       , getLocalTxns
       , mayBlockBeUseful

       -- * Operations with effects.
       , ProcessBlockRes (..)
       , processBlock
       , processNewSlot
       , processCommitment
       , processOpening
       , processShares
       , processTx
       , processVssCertificate
       ) where

import           Control.TimeWarp.Rpc (ResponseT)
import           Data.Acid            (EventResult, EventState, QueryEvent, UpdateEvent)
import           Serokell.Util        (VerificationRes)
import           Universum

import           Pos.Crypto           (PublicKey, Share)
import           Pos.Slotting         (MonadSlots, getCurrentSlot)
import           Pos.State.Acidic     (DiskState, tidyState)
import qualified Pos.State.Acidic     as A
import           Pos.State.Storage    (ProcessBlockRes (..), Storage)
import           Pos.Types            (Block, Commitment, CommitmentSignature, EpochIndex,
                                       HeaderHash, MainBlockHeader, Opening, SlotId,
                                       SlotLeaders, Tx, VssCertificate)

-- | NodeState encapsulates all the state stored by node.
type NodeState = DiskState

-- | Convenient type class to avoid passing NodeState throughout the code.
class MonadDB m where
    getNodeState :: m NodeState

instance (Monad m, MonadDB m) => MonadDB (ReaderT r m) where
    getNodeState = lift getNodeState

instance (Monad m, MonadDB m) => MonadDB (ResponseT m) where
    getNodeState = lift getNodeState

type WorkModeDB m = (MonadIO m, MonadDB m)

-- | Open NodeState, reading existing state from disk (if any).
openState :: (MonadIO m, MonadSlots m) => Bool -> FilePath -> m NodeState
openState deleteIfExists fp = openStateDo (A.openState deleteIfExists fp)

-- | Open NodeState which doesn't store anything on disk. Everything
-- is stored in memory and will be lost after shutdown.
openMemState :: (MonadIO m, MonadSlots m) => m NodeState
openMemState = openStateDo A.openMemState

openStateDo :: (MonadIO m, MonadSlots m) => m DiskState -> m NodeState
openStateDo openDiskState = do
    st <- openDiskState
    A.update st . A.ProcessNewSlot =<< getCurrentSlot
    st <$ tidyState st

-- | Safely close NodeState.
closeState :: MonadIO m => NodeState -> m ()
closeState = A.closeState

queryDisk
    :: (EventState event ~ Storage, QueryEvent event, WorkModeDB m)
    => event -> m (EventResult event)
queryDisk e = flip A.query e =<< getNodeState

updateDisk
    :: (EventState event ~ Storage, UpdateEvent event, WorkModeDB m)
    => event -> m (EventResult event)
updateDisk e = flip A.update e =<< getNodeState

-- | Get list of slot leaders for the given epoch. Empty list is returned
-- if no information is available.
getLeaders :: WorkModeDB m => EpochIndex -> m SlotLeaders
getLeaders = queryDisk . A.GetLeaders

-- | Get Block by hash.
getBlock :: WorkModeDB m => HeaderHash -> m (Maybe Block)
getBlock = queryDisk . A.GetBlock

-- | Get block which is the head of the __best chain__.
getHeadBlock :: WorkModeDB m => m Block
getHeadBlock = queryDisk A.GetHeadBlock

getLocalTxns :: WorkModeDB m => m (HashSet Tx)
getLocalTxns = queryDisk A.GetLocalTxns

mayBlockBeUseful
    :: WorkModeDB m
    => SlotId -> MainBlockHeader -> m VerificationRes
mayBlockBeUseful si = queryDisk . A.MayBlockBeUseful si

-- TODO: should 'processTx', 'processOpening', and 'processCommitment' return
-- True if the thing was valid but we already knew about it?

-- | Process transaction received from other party.
processTx :: WorkModeDB m => Tx -> m Bool
processTx = updateDisk . A.ProcessTx

-- | Notify NodeState about beginning of new slot.
processNewSlot :: WorkModeDB m => SlotId -> m ()
processNewSlot = updateDisk . A.ProcessNewSlot

-- | Process some Block received from the network.
processBlock :: WorkModeDB m => SlotId -> Block -> m ProcessBlockRes
processBlock si = updateDisk . A.ProcessBlock si

processCommitment
    :: WorkModeDB m
    => PublicKey -> (Commitment, CommitmentSignature) -> m ()
processCommitment pk c = updateDisk $ A.ProcessCommitment pk c

processOpening
    :: WorkModeDB m
    => PublicKey -> Opening -> m ()
processOpening pk o = updateDisk $ A.ProcessOpening pk o

processShares
    :: WorkModeDB m
    => PublicKey -> HashMap PublicKey Share -> m ()
processShares pk s = updateDisk $ A.ProcessShares pk s

processVssCertificate
    :: WorkModeDB m
    => PublicKey -> VssCertificate -> m ()
processVssCertificate pk c = updateDisk $ A.ProcessVssCertificate pk c
