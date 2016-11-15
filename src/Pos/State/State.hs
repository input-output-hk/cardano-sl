{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
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
       , getHeadBlock
       , getLeaders
       , getLocalTxs
       , getLocalSscPayload
       , getGlobalMpcData
       , mayBlockBeUseful

       -- * Operations with effects.
       , ProcessBlockRes (..)
       , ProcessTxRes (..)
       , createNewBlock
       --, generateAndSetNewSecret
       , processBlock
       , processNewSlot
       , processSscMessage
       , processTx

       -- * Stats collecting and fetching.
       , newStatRecord
       , getStatRecords

       -- * Functions for generating seed by SSC algorithm.
       , getThreshold
       , getParticipants

       -- * SscDynamic state simple getters and setters.
       , setToken
       , getToken
       , getOurShares
       ) where

import           Data.Acid                (EventResult, EventState, QueryEvent,
                                           UpdateEvent)
import qualified Data.Binary              as Binary
import           Data.Default             (Default)
import           Pos.DHT                  (DHTResponseT)
import           Serokell.Util            (VerificationRes, show')
import           Universum

import           Crypto.Random            (seedNew, seedToInteger)
import           Data.List.NonEmpty       (NonEmpty)
import           Pos.Crypto               (PublicKey, SecretKey, Share, Threshold,
                                           VssKeyPair, VssPublicKey)
import           Pos.Slotting             (MonadSlots, getCurrentSlot)
import           Pos.Ssc.Class.Storage    (SscStorageClass (..), SscStorageMode)
import           Pos.Ssc.Class.Types      (SscTypes (SscMessage, SscPayload, SscStorage, SscToken))
import           Pos.State.Acidic         (DiskState, tidyState)
import qualified Pos.State.Acidic         as A
import           Pos.State.Storage        (ProcessBlockRes (..), ProcessTxRes (..),
                                           Storage)
import           Pos.Statistics.StatEntry (StatLabel (..))
import           Pos.Types                (Block, EpochIndex, GenesisBlock, HeaderHash,
                                           MainBlock, MainBlockHeader, SlotId,
                                           SlotLeaders, Timestamp, Tx)

-- | NodeState encapsulates all the state stored by node.
type NodeState ssc = DiskState ssc
type QUConstraint ssc m = (SscStorageMode ssc, WorkModeDB ssc m)

-- | Convenient type class to avoid passing NodeState throughout the code.
class MonadDB ssc m | m->ssc where
    getNodeState :: m (NodeState ssc)

instance (Monad m, MonadDB ssc m) => MonadDB ssc (ReaderT r m) where
    getNodeState = lift getNodeState

instance (Monad m, MonadDB ssc m) => MonadDB ssc (DHTResponseT m) where
    getNodeState = lift getNodeState

type WorkModeDB ssc m = (MonadIO m, MonadDB ssc m)

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

-- | Get block which is the head of the __best chain__.
getHeadBlock ::  QUConstraint ssc m => m (Block ssc)
getHeadBlock = queryDisk A.GetHeadBlock

getLocalTxs :: QUConstraint ssc m => m (HashSet Tx)
getLocalTxs = queryDisk A.GetLocalTxs

getLocalSscPayload :: QUConstraint ssc m => SlotId -> m (SscPayload ssc)
getLocalSscPayload = queryDisk . A.GetLocalSscPayload

getGlobalMpcData :: QUConstraint ssc m => m (SscPayload ssc)
getGlobalMpcData = queryDisk A.GetGlobalSscPayload

mayBlockBeUseful :: QUConstraint ssc m => SlotId -> MainBlockHeader ssc -> m VerificationRes
mayBlockBeUseful si = queryDisk . A.MayBlockBeUseful si

-- | Create new block on top of currently known best chain, assuming
-- we are slot leader.
createNewBlock :: QUConstraint ssc m
               => SecretKey
               -> SlotId
               -> m (Maybe (MainBlock ssc))
createNewBlock sk = updateDisk . A.CreateNewBlock sk

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

processSscMessage :: QUConstraint ssc m => SscMessage ssc -> m (Maybe (SscMessage ssc))
processSscMessage = updateDisk . A.ProcessSscMessage


-- | Functions for collecting stats (for benchmarking)
getStatRecords :: (QUConstraint ssc m, StatLabel l)
               => l
               -> m (Maybe [(Timestamp, EntryType l)])
getStatRecords label = fmap toEntries <$> queryDisk (A.GetStatRecords $ show' label)
  where toEntries = map $ bimap fromIntegral Binary.decode

newStatRecord :: (QUConstraint ssc m, StatLabel l)
              => l
              -> Timestamp
              -> EntryType l
              -> m ()
newStatRecord label ts entry =
    updateDisk $ A.NewStatRecord (show' label) (fromIntegral ts) $ Binary.encode entry

-- | Functions for generating seed by SSC algorithm
getParticipants
    :: QUConstraint ssc m
    => EpochIndex
    -> m (Maybe (NonEmpty VssPublicKey))
getParticipants = queryDisk . A.GetParticipants

getThreshold :: QUConstraint ssc m
             => EpochIndex
             -> m (Maybe Threshold)
getThreshold = queryDisk . A.GetThreshold


----------------------------------------------------------------------------
-- Functions related to SscDynamicState
----------------------------------------------------------------------------
getToken
    :: forall ssc. forall m.
       QUConstraint ssc m
    => m (Maybe (SscToken ssc))
getToken = queryDisk @ssc A.GetToken

setToken
    :: forall ssc. forall m.
       QUConstraint ssc m
    => SscToken ssc -> m ()
setToken = updateDisk @ssc . A.SetToken

getOurShares
    :: forall ssc. forall m.
       QUConstraint ssc m
    => VssKeyPair -> m (HashMap PublicKey Share)
getOurShares ourKey = do
    randSeed <- liftIO seedNew
    queryDisk @ssc $ A.GetOurShares ourKey (seedToInteger randSeed)
