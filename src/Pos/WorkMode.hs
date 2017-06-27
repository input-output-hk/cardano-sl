{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Pos.WorkMode
       ( WorkMode
       , MinWorkMode

       , TxpExtra_TMP

       -- * Actual modes
       , RealMode
       , RealModeContext(..)
       ) where

import           Universum

import qualified Control.Monad.Reader        as Mtl
import           EtherCompat
import           Mockable                    (Production, SharedAtomicT)
import           System.Wlog                 (HasLoggerName (..), LoggerName)

import           Pos.Block.BListener         (MonadBListener (..), onApplyBlocksStub,
                                              onRollbackBlocksStub)
import           Pos.Block.Core              (Block, BlockHeader)
import           Pos.Block.Types             (Undo)
import           Pos.Communication.PeerState (HasPeerState (..), PeerStateCtx,
                                              WithPeerState (..), clearPeerStateDefault,
                                              getAllStatesDefault, getPeerStateDefault)
import           Pos.Context                 (HasNodeContext (..), HasPrimaryKey (..),
                                              HasSscContext (..), NodeContext)
import           Pos.Core                    (IsHeader)
import           Pos.DB                      (MonadGState (..), NodeDBs)
import           Pos.DB.Block                (MonadBlockDBWrite (..), dbGetBlockDefault,
                                              dbGetBlockSscDefault, dbGetHeaderDefault,
                                              dbGetHeaderSscDefault, dbGetUndoDefault,
                                              dbGetUndoSscDefault, dbPutBlundDefault)
import           Pos.DB.Class                (MonadBlockDBGeneric (..), MonadDB (..),
                                              MonadDBRead (..))
import           Pos.DB.DB                   (gsAdoptedBVDataDefault)
import           Pos.DB.Redirect             (dbDeleteDefault, dbGetDefault,
                                              dbIterSourceDefault, dbPutDefault,
                                              dbWriteBatchDefault)
import           Pos.Delegation.Class        (DelegationVar)
import           Pos.Discovery               (HasDiscoveryContextSum (..),
                                              MonadDiscovery (..), findPeersSum,
                                              getPeersSum)
import           Pos.ExecMode.Context        ((:::), modeContext)
import           Pos.Reporting               (HasReportingContext (..))
import           Pos.Shutdown                (HasShutdownContext (..))
import           Pos.Slotting.Class          (MonadSlots (..))
import           Pos.Slotting.Impl.Sum       (currentTimeSlottingSum,
                                              getCurrentSlotBlockingSum,
                                              getCurrentSlotInaccurateSum,
                                              getCurrentSlotSum)
import           Pos.Slotting.MemState       (HasSlottingVar (..), MonadSlotsData (..),
                                              getSlottingDataDefault,
                                              getSystemStartDefault,
                                              putSlottingDataDefault,
                                              waitPenultEpochEqualsDefault)
import           Pos.Ssc.Class.Helpers       (SscHelpersClass)
import           Pos.Ssc.Class.Types         (SscBlock)
import           Pos.Ssc.Extra               (SscMemTag, SscState)
import           Pos.Txp.MemState            (GenericTxpLocalData, TxpHolderTag)
import           Pos.Util                    (Some (..))
import           Pos.Util.JsonLog            (JsonLogConfig, jsonLogDefault)
import           Pos.Util.TimeWarp           (CanJsonLog (..))
import           Pos.Util.UserSecret         (HasUserSecret (..))
import           Pos.WorkMode.Class          (MinWorkMode, TxpExtra_TMP, WorkMode)

data PeerStateTag

modeContext [d|
    data RealModeContext ssc = RealModeContext
        !(NodeDBs       ::: NodeDBs)
        !(SscMemTag     ::: SscState ssc)
        !(TxpHolderTag  ::: GenericTxpLocalData TxpExtra_TMP)
        !(DelegationVar ::: DelegationVar)
        !(PeerStateTag  ::: PeerStateCtx Production)
        !(JsonLogConfig ::: JsonLogConfig)
        !(LoggerName    ::: LoggerName)
        !(NodeContext ssc)
    |]

rmcNodeContext :: Lens' (RealModeContext ssc) (NodeContext ssc)
rmcNodeContext f (RealModeContext x1 x2 x3 x4 x5 x6 x7 nc) =
    RealModeContext x1 x2 x3 x4 x5 x6 x7 <$> f nc

instance HasSscContext ssc (RealModeContext ssc) where
    sscContext = rmcNodeContext . sscContext

instance HasPrimaryKey (RealModeContext ssc) where
    primaryKey = rmcNodeContext . primaryKey

instance HasDiscoveryContextSum (RealModeContext ssc) where
    discoveryContextSum = rmcNodeContext . discoveryContextSum

instance HasReportingContext (RealModeContext ssc) where
    reportingContext = rmcNodeContext . reportingContext

instance HasUserSecret (RealModeContext ssc) where
    userSecret = rmcNodeContext . userSecret

instance HasShutdownContext (RealModeContext ssc) where
    shutdownContext = rmcNodeContext . shutdownContext

instance HasSlottingVar (RealModeContext ssc) where
    slottingTimestamp = rmcNodeContext . slottingTimestamp
    slottingVar = rmcNodeContext . slottingVar

instance HasNodeContext ssc (RealModeContext ssc) where
    nodeContext = rmcNodeContext

instance sa ~ SharedAtomicT Production => HasPeerState sa (RealModeContext ssc) where
    peerState = lensOf @PeerStateTag

type RealMode ssc = Mtl.ReaderT (RealModeContext ssc) Production

instance {-# OVERLAPPING #-} HasLoggerName (RealMode ssc) where
    getLoggerName = view (lensOf @LoggerName)
    modifyLoggerName f = local (lensOf @LoggerName %~ f)

instance {-# OVERLAPPING #-} CanJsonLog (RealMode ssc) where
    jsonLog = jsonLogDefault

instance MonadSlotsData (RealMode ssc) where
    getSystemStart = getSystemStartDefault
    getSlottingData = getSlottingDataDefault
    waitPenultEpochEquals = waitPenultEpochEqualsDefault
    putSlottingData = putSlottingDataDefault

instance MonadSlots (RealMode ssc) where
    getCurrentSlot = getCurrentSlotSum
    getCurrentSlotBlocking = getCurrentSlotBlockingSum
    getCurrentSlotInaccurate = getCurrentSlotInaccurateSum
    currentTimeSlotting = currentTimeSlottingSum

instance MonadDiscovery (RealMode ssc) where
    getPeers = getPeersSum
    findPeers = findPeersSum

instance MonadGState (RealMode ssc) where
    gsAdoptedBVData = gsAdoptedBVDataDefault

instance MonadDBRead (RealMode ssc) where
    dbGet = dbGetDefault
    dbIterSource = dbIterSourceDefault

instance MonadDB (RealMode ssc) where
    dbPut = dbPutDefault
    dbWriteBatch = dbWriteBatchDefault
    dbDelete = dbDeleteDefault

instance SscHelpersClass ssc => MonadBlockDBWrite ssc (RealMode ssc) where
    dbPutBlund = dbPutBlundDefault

instance MonadBListener (RealMode ssc) where
    onApplyBlocks = onApplyBlocksStub
    onRollbackBlocks = onRollbackBlocksStub

instance WithPeerState (RealMode ssc) where
    getPeerState = getPeerStateDefault
    clearPeerState = clearPeerStateDefault
    getAllStates = getAllStatesDefault

instance
    SscHelpersClass ssc =>
    MonadBlockDBGeneric (BlockHeader ssc) (Block ssc) Undo (RealMode ssc)
  where
    dbGetBlock  = dbGetBlockDefault @ssc
    dbGetUndo   = dbGetUndoDefault @ssc
    dbGetHeader = dbGetHeaderDefault @ssc

instance
    SscHelpersClass ssc =>
    MonadBlockDBGeneric (Some IsHeader) (SscBlock ssc) () (RealMode ssc)
  where
    dbGetBlock  = dbGetBlockSscDefault @ssc
    dbGetUndo   = dbGetUndoSscDefault @ssc
    dbGetHeader = dbGetHeaderSscDefault @ssc
