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
       , unRealMode
       , RealModeContext(..)
       ) where

import qualified Control.Monad.Reader        as Mtl
import qualified Ether
import           Mockable.Production         (Production)
import           System.Wlog                 (HasLoggerName (..), LoggerName)

import           Pos.Block.BListener         (MonadBListener (..), onApplyBlocksStub,
                                              onRollbackBlocksStub)
import           Pos.Block.Core              (Block, BlockHeader)
import           Pos.Block.Types             (Undo)
import           Pos.Communication.PeerState (PeerStateCtx, PeerStateTag,
                                              WithPeerState (..), clearPeerStateReal,
                                              getAllStatesReal, getPeerStateReal)
import           Pos.Context                 (NodeContext)
import           Pos.Core                    (IsHeader)
import           Pos.DB                      (MonadGState (..), NodeDBs)
import           Pos.DB.Block                (MonadBlockDBWrite (..), dbGetBlockReal,
                                              dbGetBlockReal', dbGetHeaderReal,
                                              dbGetHeaderReal', dbGetUndoReal,
                                              dbGetUndoReal', dbPutBlundReal)
import           Pos.DB.Class                (MonadBlockDBGeneric (..), MonadDB (..),
                                              MonadDBRead (..))
import           Pos.DB.DB                   (gsAdoptedBVDataDB)
import           Pos.DB.Redirect             (dbDeleteReal, dbGetReal, dbPutReal,
                                              dbWriteBatchReal)
import           Pos.Delegation.Class        (DelegationVar)
import           Pos.Discovery               (MonadDiscovery (..), findPeersReal,
                                              getPeersReal)
import           Pos.ExecMode                (ExecMode (..), ExecModeM, modeContext, (:::))
import           Pos.Slotting.Class          (MonadSlots (..))
import           Pos.Slotting.Impl.Sum       (currentTimeSlottingReal,
                                              getCurrentSlotBlockingReal,
                                              getCurrentSlotInaccurateReal,
                                              getCurrentSlotReal)
import           Pos.Slotting.MemState       (MonadSlotsData (..), getSlottingDataReal,
                                              getSystemStartReal, putSlottingDataReal,
                                              waitPenultEpochEqualsReal)
import           Pos.Ssc.Class.Helpers       (SscHelpersClass)
import           Pos.Ssc.Class.Types         (SscBlock)
import           Pos.Ssc.Extra               (SscMemTag, SscState)
import           Pos.Txp.MemState            (GenericTxpLocalData, TxpHolderTag)
import           Pos.Util                    (Some (..))
import           Pos.Util.JsonLog            (JsonLogConfig, jsonLogReal)
import           Pos.Util.TimeWarp           (CanJsonLog (..))
import           Pos.WorkMode.Class          (MinWorkMode, TxpExtra_TMP, WorkMode)

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

data REAL ssc

type RealMode ssc = ExecMode (REAL ssc)

type instance ExecModeM (REAL ssc) =
    Mtl.ReaderT (RealModeContext ssc) Production

unRealMode :: ExecMode (REAL ssc) a -> ExecModeM (REAL ssc) a
unRealMode = unExecMode

instance HasLoggerName (RealMode ssc) where
    getLoggerName = Ether.ask'
    modifyLoggerName = Ether.local'

instance CanJsonLog (RealMode ssc) where
    jsonLog = jsonLogReal

instance MonadSlotsData (RealMode ssc) where
    getSystemStart = getSystemStartReal
    getSlottingData = getSlottingDataReal
    waitPenultEpochEquals = waitPenultEpochEqualsReal
    putSlottingData = putSlottingDataReal

instance MonadSlots (RealMode ssc) where
    getCurrentSlot = getCurrentSlotReal
    getCurrentSlotBlocking = getCurrentSlotBlockingReal
    getCurrentSlotInaccurate = getCurrentSlotInaccurateReal
    currentTimeSlotting = currentTimeSlottingReal

instance MonadDiscovery (RealMode ssc) where
    getPeers = getPeersReal
    findPeers = findPeersReal

instance MonadGState (RealMode ssc) where
    gsAdoptedBVData = gsAdoptedBVDataDB

instance MonadDBRead (RealMode ssc) where
    dbGet = dbGetReal

instance MonadDB (RealMode ssc) where
    dbPut = dbPutReal
    dbWriteBatch = dbWriteBatchReal
    dbDelete = dbDeleteReal

instance SscHelpersClass ssc => MonadBlockDBWrite ssc (RealMode ssc) where
    dbPutBlund = dbPutBlundReal

instance MonadBListener (RealMode ssc) where
    onApplyBlocks = onApplyBlocksStub
    onRollbackBlocks = onRollbackBlocksStub

instance WithPeerState (RealMode ssc) where
    getPeerState = getPeerStateReal
    clearPeerState = clearPeerStateReal
    getAllStates = getAllStatesReal

instance
    SscHelpersClass ssc =>
    MonadBlockDBGeneric (BlockHeader ssc) (Block ssc) Undo (RealMode ssc)
  where
    dbGetBlock  = dbGetBlockReal @ssc
    dbGetUndo   = dbGetUndoReal @ssc
    dbGetHeader = dbGetHeaderReal @ssc

instance
    SscHelpersClass ssc =>
    MonadBlockDBGeneric (Some IsHeader) (SscBlock ssc) () (RealMode ssc)
  where
    dbGetBlock  = dbGetBlockReal' @ssc
    dbGetUndo   = dbGetUndoReal' @ssc
    dbGetHeader = dbGetHeaderReal' @ssc
