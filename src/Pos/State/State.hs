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
       , initFirstSlot
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

       -- [CSL-103]: these function should be moved to GodTossing.
       -- * Functions for generating seed by SSC algorithm.
       , getParticipants
       , getThreshold

       -- * SscGodTossing simple getters and setters.
       , getOurShares
       ) where

import           Universum

import           Crypto.Random            (drgNewSeed, seedNew, withDRG)
import           Data.Acid                (EventResult, EventState, QueryEvent,
                                           UpdateEvent)
import           Data.Default             (Default)
import qualified Data.HashMap.Strict      as HM
import           Data.List.NonEmpty       (NonEmpty)
import           Formatting               (build, sformat, (%))
import           Pos.DHT                  (DHTResponseT)
import           Serokell.Util            (VerificationRes)
import           System.Wlog              (HasLoggerName, LogEvent, LoggerName,
                                           dispatchEvents, getLoggerName, logWarning,
                                           runPureLog, usingLoggerName)

import           Pos.Crypto               (LVssPublicKey, PublicKey, SecretKey, Share,
                                           VssKeyPair, decryptShare, toVssPublicKey)
import           Pos.Slotting             (MonadSlots, getCurrentSlot)
import           Pos.Ssc.Class.Storage    (SscStorageClass (..), SscStorageMode)
import           Pos.Ssc.Class.Types      (Ssc (SscGlobalState, SscPayload, SscStorage))
import           Pos.State.Acidic         (DiskState, tidyState)
import qualified Pos.State.Acidic         as A
import           Pos.State.Storage        (ProcessBlockRes (..), ProcessTxRes (..),
                                           Storage, getThreshold)
import           Pos.Statistics.StatEntry ()
import           Pos.Types                (Block, EpochIndex, GenesisBlock, HeaderHash,
                                           MainBlock, MainBlockHeader, SlotId,
                                           SlotLeaders, Tx)
import           Pos.Util                 (deserializeM, serialize)

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

type QUConstraint  ssc m = (SscStorageMode ssc, WorkModeDB ssc m)
type QULConstraint ssc m = (SscStorageMode ssc, WorkModeDB ssc m, HasLoggerName m)

-- | Open NodeState, reading existing state from disk (if any).
openState
    :: (SscStorageMode ssc, Default (SscStorage ssc),
        MonadIO m)
    => Maybe (Storage ssc)
    -> Bool
    -> FilePath
    -> m (NodeState ssc)
openState storage deleteIfExists fp =
    maybe (A.openState deleteIfExists fp)
        (\s -> A.openStateCustom s deleteIfExists fp)
        storage

-- | Open NodeState which doesn't store anything on disk. Everything
-- is stored in memory and will be lost after shutdown.
openMemState
    :: (SscStorageMode ssc, Default (SscStorage ssc),
        MonadIO m)
    => Maybe (Storage ssc)
    -> m (NodeState ssc)
openMemState = maybe A.openMemState A.openMemStateCustom

initFirstSlot
    :: forall ssc m .
       (MonadIO m, MonadSlots m, SscStorageMode ssc
       , HasLoggerName m
       , WorkModeDB ssc m)
    => m ()
initFirstSlot = do
    st <- getNodeState
    _  <- A.updateWithLog st . A.ProcessNewSlotL =<< getCurrentSlot
    tidyState st

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

updateDiskWithLog
     :: ( SscStorageClass ssc
        , EventState event ~ (Storage ssc)
        , EventResult event ~ (a, [LogEvent])
        , UpdateEvent event
        , WorkModeDB ssc m
        , HasLoggerName m)
     => (LoggerName -> event)
     -> m (a, [LogEvent])
updateDiskWithLog le = flip A.updateWithLog le =<< getNodeState

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
getGlobalMpcData = queryDisk A.GetGlobalSscState

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
               -> m (Either Text (MainBlock ssc))
createNewBlock sk si = updateDisk . A.CreateNewBlock sk si

-- | Process transaction received from other party.
processTx :: QUConstraint ssc m => Tx -> m ProcessTxRes
processTx = updateDisk . A.ProcessTx

-- | Notify NodeState about beginning of new slot. Ideally it should
-- be used before all other updates within this slot.
processNewSlot :: QULConstraint ssc m => SlotId -> m (Maybe (GenesisBlock ssc), [LogEvent])
processNewSlot = updateDiskWithLog . A.ProcessNewSlotL

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
    -> m (Maybe (NonEmpty LVssPublicKey))
getParticipants = queryDisk . A.GetParticipants

----------------------------------------------------------------------------
-- Related to SscGodTossing
----------------------------------------------------------------------------

-- | Decrypt shares (in commitments) that are intended for us and that we can
-- decrypt.
getOurShares
    :: QULConstraint ssc m
    => VssKeyPair -> m (HashMap PublicKey Share)
getOurShares ourKey = do
    randSeed <- liftIO seedNew
    let ourPK = serialize $ toVssPublicKey ourKey
    encSharesM <- queryDisk $ A.GetOurShares ourPK
    let drg = drgNewSeed randSeed
        (res, pLog) = fst . withDRG drg . runPureLog . usingLoggerName mempty <$>
                        flip traverse (HM.toList encSharesM) $ \(pk, lEncSh) -> do
                          let mEncSh = deserializeM lEncSh
                          case mEncSh of
                            Just encShare -> lift . lift $ Just . (,) pk <$> decryptShare ourKey encShare
                            _             -> do
                                logWarning $
                                    sformat ("Failed to deserialize share for " % build) pk
                                return Nothing
        resHM = HM.fromList . catMaybes $ res
    loggerName <- getLoggerName
    liftIO $ usingLoggerName loggerName $ dispatchEvents pLog
    return resHM
