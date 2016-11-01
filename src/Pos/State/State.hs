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
       , getLocalTxs
       , getLocalMpcData
       , getGlobalMpcData
       , getSecret
       , getOurCommitment
       , getOurOpening
       , getOurShares
       , mayBlockBeUseful

       -- * Operations with effects.
       , ProcessBlockRes (..)
       , createNewBlock
       , processBlock
       , processNewSlot
       , processCommitment
       , processOpening
       , processShares
       , processTx
       , processVssCertificate
       , generateNewSecret

       -- * Stats collecting and fetching
       , addStatRecord
       , getStatRecords
       ) where

import           Crypto.Random              (seedNew, seedToInteger)
import           Data.Acid                  (EventResult, EventState, QueryEvent,
                                             UpdateEvent)
import           Data.Binary                (Binary)
import qualified Data.Binary                as Binary
import           Pos.DHT                    (DHTResponseT)
import           Serokell.Util              (VerificationRes)
import           Universum

import           Pos.Crypto                 (PublicKey, SecretKey, Share, VssKeyPair,
                                             toPublic)
import           Pos.Slotting               (MonadSlots, getCurrentSlot)
import           Pos.Ssc.DynamicState.Types (DSPayload, SscDynamicState)
import           Pos.State.Acidic           (DiskState, tidyState)
import qualified Pos.State.Acidic           as A
import           Pos.State.Storage          (IdTimestamp (..), ProcessBlockRes (..),
                                             Storage)
import           Pos.Types                  (Block, Commitment, CommitmentSignature,
                                             EpochIndex, HeaderHash, MainBlock,
                                             MainBlockHeader, Opening, SignedCommitment,
                                             SlotId, SlotLeaders, Timestamp, Tx,
                                             VssCertificate, genCommitmentAndOpening,
                                             mkSignedCommitment)

-- | NodeState encapsulates all the state stored by node.
type NodeState = DiskState

-- | Convenient type class to avoid passing NodeState throughout the code.
class MonadDB m where
    getNodeState :: m NodeState

instance (Monad m, MonadDB m) => MonadDB (ReaderT r m) where
    getNodeState = lift getNodeState

instance (Monad m, MonadDB m) => MonadDB (DHTResponseT m) where
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
getLeaders :: WorkModeDB m => EpochIndex -> m (Maybe SlotLeaders)
getLeaders = queryDisk . A.GetLeaders

-- | Get Block by hash.
getBlock
    :: WorkModeDB m
    => HeaderHash SscDynamicState -> m (Maybe (Block SscDynamicState))
getBlock = queryDisk . A.GetBlock

-- | Get block which is the head of the __best chain__.
getHeadBlock :: WorkModeDB m => m (Block SscDynamicState)
getHeadBlock = queryDisk A.GetHeadBlock

getLocalTxs :: WorkModeDB m => m (HashSet Tx)
getLocalTxs = queryDisk A.GetLocalTxs

getLocalMpcData :: WorkModeDB m => m DSPayload
getLocalMpcData = queryDisk A.GetLocalMpcData

getGlobalMpcData :: WorkModeDB m => m DSPayload
getGlobalMpcData = queryDisk A.GetGlobalMpcData

mayBlockBeUseful
    :: WorkModeDB m
    => SlotId -> MainBlockHeader SscDynamicState -> m VerificationRes
mayBlockBeUseful si = queryDisk . A.MayBlockBeUseful si

-- | Create new block on top of currently known best chain, assuming
-- we are slot leader.
createNewBlock
    :: WorkModeDB m
    => SecretKey -> SlotId -> m (Maybe (MainBlock SscDynamicState))
createNewBlock sk = updateDisk . A.CreateNewBlock sk

-- | Process transaction received from other party.
processTx :: WorkModeDB m => Tx -> m Bool
processTx = updateDisk . A.ProcessTx

-- | Notify NodeState about beginning of new slot.
processNewSlot :: WorkModeDB m => SlotId -> m ()
processNewSlot = updateDisk . A.ProcessNewSlot

-- | Process some Block received from the network.
processBlock
    :: WorkModeDB m
    => SlotId -> Block SscDynamicState -> m (ProcessBlockRes SscDynamicState)
processBlock si = updateDisk . A.ProcessBlock si

processCommitment
    :: WorkModeDB m
    => PublicKey -> SignedCommitment -> m Bool
processCommitment pk c = updateDisk $ A.ProcessCommitment pk c

processOpening
    :: WorkModeDB m
    => PublicKey -> Opening -> m Bool
processOpening pk o = updateDisk $ A.ProcessOpening pk o

processShares
    :: WorkModeDB m
    => PublicKey -> HashMap PublicKey Share -> m Bool
processShares pk s = updateDisk $ A.ProcessShares pk s

processVssCertificate
    :: WorkModeDB m
    => PublicKey -> VssCertificate -> m ()
processVssCertificate pk c = updateDisk $ A.ProcessVssCertificate pk c

-- | Generate new commitment and opening and use them for the current
-- epoch. Assumes that the genesis block has already been generated and
-- processed by MPC (when the genesis block is processed, the secret is
-- cleared) (otherwise 'generateNewSecret' will fail because 'A.SetSecret'
-- won't set the secret if there's one already).
generateNewSecret
    :: WorkModeDB m
    => SecretKey
    -> EpochIndex                         -- ^ Current epoch
    -> m (SignedCommitment, Opening)
generateNewSecret sk epoch = do
    -- TODO: I think it's safe here to perform 3 operations which aren't
    -- grouped into a single transaction here, but I'm still a bit nervous.
    threshold <- queryDisk (A.GetThreshold epoch)
    -- FIXME
    participants <- fromMaybe undefined <$> queryDisk (A.GetParticipants epoch)
    secret <-
        first (mkSignedCommitment sk epoch) <$>
        genCommitmentAndOpening threshold participants
    secret <$ updateDisk (A.SetSecret (toPublic sk) secret)

getSecret :: WorkModeDB m => m (Maybe (PublicKey, SignedCommitment, Opening))
getSecret = queryDisk A.GetSecret

getOurCommitment :: WorkModeDB m => m (Maybe SignedCommitment)
getOurCommitment = queryDisk A.GetOurCommitment

getOurOpening :: WorkModeDB m => m (Maybe Opening)
getOurOpening = queryDisk A.GetOurOpening

getOurShares :: WorkModeDB m => VssKeyPair -> m (HashMap PublicKey Share)
getOurShares ourKey = do
    randSeed <- liftIO seedNew
    queryDisk $ A.GetOurShares ourKey (seedToInteger randSeed)

-- | Functions for collecting stats (for benchmarking)
toPair :: Binary a => IdTimestamp -> (a, Timestamp)
toPair IdTimestamp {..} = (Binary.decode itId, fromIntegral itTimestamp)

fromArgs :: Binary a => a -> Timestamp -> IdTimestamp
fromArgs id time = IdTimestamp (Binary.encode id) (fromIntegral time)

getStatRecords :: (WorkModeDB m, Binary a) => Text -> m (Maybe [(a, Timestamp)])
getStatRecords label = fmap (map toPair) <$> queryDisk (A.GetStatRecords label)

addStatRecord :: (WorkModeDB m, Binary a) => Text -> a -> Timestamp -> m ()
addStatRecord label id time = updateDisk $ A.AddStatRecord label $ fromArgs id time
