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
       , getLocalSscPayload
       , getGlobalMpcData
       , getSecret
       , getOurCommitment
       , getOurOpening
       , getOurShares
       , mayBlockBeUseful

       -- * Operations with effects.
       , ProcessBlockRes (..)
       , ProcessTxRes (..)
       , createNewBlock
       , generateAndSetNewSecret
       , processBlock
       , processNewSlot
       , processSscMessage
       , processTx

       -- * Stats collecting and fetching
       , addStatRecord
       , getStatRecords
       ) where

import           Control.Lens               (view, _2, _3)
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
import           Pos.Ssc.Class.Types        (SscTypes (SscMessage))
import           Pos.Ssc.DynamicState.Types (DSPayload, SscDynamicState)
import           Pos.State.Acidic           (DiskState, tidyState)
import qualified Pos.State.Acidic           as A
import           Pos.State.Storage          (IdTimestamp (..), ProcessBlockRes (..),
                                             ProcessTxRes (..), Storage)
import           Pos.Types                  (Block, EpochIndex, GenesisBlock, HeaderHash,
                                             MainBlock, MainBlockHeader, Opening,
                                             SignedCommitment, SlotId, SlotLeaders,
                                             Timestamp, Tx, genCommitmentAndOpening,
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
openState
    :: (MonadIO m, MonadSlots m)
    => Maybe Storage -> Bool -> FilePath -> m NodeState
openState storage deleteIfExists fp =
    openStateDo $ maybe (A.openState deleteIfExists fp)
                        (\s -> A.openStateCustom s deleteIfExists fp)
                        storage

-- | Open NodeState which doesn't store anything on disk. Everything
-- is stored in memory and will be lost after shutdown.
openMemState :: (MonadIO m, MonadSlots m) => Maybe Storage -> m NodeState
openMemState = openStateDo . maybe A.openMemState A.openMemStateCustom

openStateDo :: (MonadIO m, MonadSlots m) => m DiskState -> m NodeState
openStateDo openDiskState = do
    st <- openDiskState
    _ <- A.update st . A.ProcessNewSlot =<< getCurrentSlot
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

getLocalSscPayload :: WorkModeDB m => m DSPayload
getLocalSscPayload = queryDisk A.GetLocalSscPayload

getGlobalMpcData :: WorkModeDB m => m DSPayload
getGlobalMpcData = queryDisk A.GetGlobalSscPayload

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
processTx :: WorkModeDB m => Tx -> m ProcessTxRes
processTx = updateDisk . A.ProcessTx

-- | Notify NodeState about beginning of new slot. Ideally it should
-- be used before all other updates within this slot.
processNewSlot
    :: WorkModeDB m
    => SlotId -> m (Maybe (GenesisBlock SscDynamicState))
processNewSlot = updateDisk . A.ProcessNewSlot

-- | Process some Block received from the network.
processBlock
    :: WorkModeDB m
    => SlotId -> Block SscDynamicState -> m (ProcessBlockRes SscDynamicState)
processBlock si = updateDisk . A.ProcessBlock si

processSscMessage
    :: WorkModeDB m
    => SscMessage SscDynamicState -> m Bool
processSscMessage = updateDisk . A.ProcessSscMessage

-- | Generate new commitment and opening and use them for the current
-- epoch. Assumes that the genesis block has already been generated and
-- processed by MPC (when the genesis block is processed, the secret is
-- cleared) (otherwise 'generateNewSecret' will fail because 'A.SetSecret'
-- won't set the secret if there's one already).
-- Nothing is returned if node is not ready.
generateAndSetNewSecret
    :: WorkModeDB m
    => SecretKey
    -> EpochIndex                         -- ^ Current epoch
    -> m (Maybe (SignedCommitment, Opening))
generateAndSetNewSecret sk epoch = do
    -- TODO: I think it's safe here to perform 3 operations which aren't
    -- grouped into a single transaction here, but I'm still a bit nervous.
    threshold <- queryDisk (A.GetThreshold epoch)
    participants <- queryDisk (A.GetParticipants epoch)
    case (,) <$> threshold <*> participants of
        Nothing -> return Nothing
        Just (th, ps) -> do
            (comm, op) <-
                first (mkSignedCommitment sk epoch) <$>
                genCommitmentAndOpening th ps
            Just (comm, op) <$ updateDisk (A.SetToken (toPublic sk, comm, op))

getSecret :: WorkModeDB m => m (Maybe (PublicKey, SignedCommitment, Opening))
getSecret = queryDisk A.GetToken

getOurCommitment :: WorkModeDB m => m (Maybe SignedCommitment)
getOurCommitment = fmap (view _2) <$> getSecret

getOurOpening :: WorkModeDB m => m (Maybe Opening)
getOurOpening = fmap (view _3) <$> getSecret

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
