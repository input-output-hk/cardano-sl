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
                                              WithPeerState (..), clearPeerStateDefault,
                                              getAllStatesDefault, getPeerStateDefault)
import           Pos.Context                 (NodeContext)
import           Pos.Core                    (IsHeader)
import           Pos.DB                      (MonadGState (..), NodeDBs)
import           Pos.DB.Block                (MonadBlockDBWrite (..), dbGetBlockDefault,
                                              dbGetBlockSscDefault, dbGetHeaderDefault,
                                              dbGetHeaderSscDefault, dbGetUndoDefault,
                                              dbGetUndoSscDefault, dbPutBlundDefault)
import           Pos.DB.Class                (MonadBlockDBGeneric (..), MonadDB (..),
                                              MonadDBRead (..))
import           Pos.DB.DB                   (gsAdoptedBVDataDefault)
import           Pos.DB.Redirect             (dbDeleteDefault, dbGetDefault, dbPutDefault,
                                              dbWriteBatchDefault)
import           Pos.Delegation.Class        (DelegationVar)
import           Pos.Discovery               (MonadDiscovery (..), findPeersSum,
                                              getPeersSum)
import           Pos.ExecMode                ((:::), ExecMode (..), ExecModeM,
                                              modeContext)
import           Pos.Slotting.Class          (MonadSlots (..))
import           Pos.Slotting.Impl.Sum       (currentTimeSlottingSum,
                                              getCurrentSlotBlockingSum,
                                              getCurrentSlotInaccurateSum,
                                              getCurrentSlotSum)
import           Pos.Slotting.MemState       (MonadSlotsData (..), getSlottingDataDefault,
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
