{-# LANGUAGE CPP           #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS -fno-warn-unused-top-binds #-} -- for lenses

module Pos.WorkMode
       ( WorkMode
       , MinWorkMode

       , TxpExtra_TMP

       -- * Actual modes
       , RealMode
       , RealModeContext(..)

       , OQ
       , EnqueuedConversation (..)
       ) where

import           Universum

import           Control.Lens           (makeLensesWith)
import qualified Control.Monad.Reader   as Mtl
import           Ether.Internal         (HasLens (..))
import           Mockable               (Production)
import           System.Wlog            (HasLoggerName (..), LoggerName)

import           Pos.Block.BListener    (MonadBListener (..), onApplyBlocksStub,
                                         onRollbackBlocksStub)
import           Pos.Block.Core         (Block, BlockHeader)
import           Pos.Block.Slog.Types   (HasSlogContext (..), HasSlogGState (..))
import           Pos.Block.Types        (Undo)
import           Pos.Context            (HasNodeContext (..), HasPrimaryKey (..),
                                         HasSscContext (..), NodeContext)
import           Pos.Core               (HasConfiguration, IsHeader)
import           Pos.DB                 (MonadGState (..), NodeDBs)
import           Pos.DB.Block           (dbGetBlockDefault, dbGetBlockSscDefault,
                                         dbGetHeaderDefault, dbGetHeaderSscDefault,
                                         dbGetUndoDefault, dbGetUndoSscDefault,
                                         dbPutBlundDefault)
import           Pos.DB.Class           (MonadBlockDBGeneric (..),
                                         MonadBlockDBGenericWrite (..), MonadDB (..),
                                         MonadDBRead (..))
import           Pos.DHT.Real.Types     (KademliaDHTInstance)
import           Pos.Network.Types      (HasNodeType (..), getNodeTypeDefault)
import           Pos.DB.DB              (gsAdoptedBVDataDefault)
import           Pos.DB.Rocks           (dbDeleteDefault, dbGetDefault,
                                         dbIterSourceDefault, dbPutDefault,
                                         dbWriteBatchDefault)
import           Pos.Delegation.Class   (DelegationVar)
import           Pos.Infra.Configuration (HasInfraConfiguration)
import           Pos.KnownPeers         (MonadFormatPeers (..), MonadKnownPeers (..))
import           Pos.Reporting          (HasReportingContext (..))
import           Pos.Shutdown           (HasShutdownContext (..))
import           Pos.Slotting.Class     (MonadSlots (..))
import           Pos.Slotting.Impl.Sum  (currentTimeSlottingSum,
                                         getCurrentSlotBlockingSum,
                                         getCurrentSlotInaccurateSum, getCurrentSlotSum)
import           Pos.Slotting.MemState  (HasSlottingVar (..), MonadSlotsData)
import           Pos.Ssc.Class.Helpers  (SscHelpersClass)
import           Pos.Ssc.Class.Types    (SscBlock)
import           Pos.Ssc.Extra          (SscMemTag, SscState)
import           Pos.Txp.MemState       (GenericTxpLocalData, TxpHolderTag)
import           Pos.Util               (Some (..))
import           Pos.Util.JsonLog       (HasJsonLogConfig (..), JsonLogConfig,
                                         jsonLogDefault)
import           Pos.Util.LoggerName    (HasLoggerName' (..), getLoggerNameDefault,
                                         modifyLoggerNameDefault)
import           Pos.Util.OutboundQueue (EnqueuedConversation (..), OQ)
import qualified Pos.Util.OutboundQueue as OQ.Reader
import           Pos.Util.TimeWarp      (CanJsonLog (..))
import           Pos.Util.UserSecret    (HasUserSecret (..))
import           Pos.Util.Util          (postfixLFields)
import           Pos.WorkMode.Class     (MinWorkMode, TxpExtra_TMP, WorkMode)


data RealModeContext ssc = RealModeContext
    { rmcNodeDBs       :: !NodeDBs
    , rmcSscState      :: !(SscState ssc)
    , rmcTxpLocalData  :: !(GenericTxpLocalData TxpExtra_TMP)
    , rmcDelegationVar :: !DelegationVar
    , rmcJsonLogConfig :: !JsonLogConfig
    , rmcLoggerName    :: !LoggerName
    , rmcNodeContext   :: !(NodeContext ssc)
    , rmcOutboundQ     :: !(OQ (RealMode ssc))
    }

type RealMode ssc = Mtl.ReaderT (RealModeContext ssc) Production

makeLensesWith postfixLFields ''RealModeContext

instance HasLens NodeDBs (RealModeContext ssc) NodeDBs where
    lensOf = rmcNodeDBs_L

instance HasLens SscMemTag (RealModeContext ssc) (SscState ssc) where
    lensOf = rmcSscState_L

instance HasLens TxpHolderTag (RealModeContext ssc) (GenericTxpLocalData TxpExtra_TMP) where
    lensOf = rmcTxpLocalData_L

instance HasLens DelegationVar (RealModeContext ssc) DelegationVar where
    lensOf = rmcDelegationVar_L

instance HasNodeType (RealModeContext ssc) where
    getNodeType = getNodeTypeDefault @KademliaDHTInstance

instance {-# OVERLAPPABLE #-}
    HasLens tag (NodeContext ssc) r =>
    HasLens tag (RealModeContext ssc) r
  where
    lensOf = rmcNodeContext_L . lensOf @tag

instance HasSscContext ssc (RealModeContext ssc) where
    sscContext = rmcNodeContext_L . sscContext

instance HasPrimaryKey (RealModeContext ssc) where
    primaryKey = rmcNodeContext_L . primaryKey

instance HasReportingContext (RealModeContext ssc) where
    reportingContext = rmcNodeContext_L . reportingContext

instance HasUserSecret (RealModeContext ssc) where
    userSecret = rmcNodeContext_L . userSecret

instance HasShutdownContext (RealModeContext ssc) where
    shutdownContext = rmcNodeContext_L . shutdownContext

instance HasSlottingVar (RealModeContext ssc) where
    slottingTimestamp = rmcNodeContext_L . slottingTimestamp
    slottingVar = rmcNodeContext_L . slottingVar

instance HasSlogContext (RealModeContext ssc) where
    slogContext = rmcNodeContext_L . slogContext

instance HasSlogGState (RealModeContext ssc) where
    slogGState = slogContext . scGState

instance HasNodeContext ssc (RealModeContext ssc) where
    nodeContext = rmcNodeContext_L

instance HasLoggerName' (RealModeContext ssc) where
    loggerName = rmcLoggerName_L

instance HasJsonLogConfig (RealModeContext ssc) where
    jsonLogConfig = rmcJsonLogConfig_L

instance {-# OVERLAPPING #-} HasLoggerName (RealMode ssc) where
    getLoggerName = getLoggerNameDefault
    modifyLoggerName = modifyLoggerNameDefault

instance {-# OVERLAPPING #-} CanJsonLog (RealMode ssc) where
    jsonLog = jsonLogDefault

instance (HasConfiguration, HasInfraConfiguration, MonadSlotsData ctx (RealMode ssc))
      => MonadSlots ctx (RealMode ssc)
  where
    getCurrentSlot = getCurrentSlotSum
    getCurrentSlotBlocking = getCurrentSlotBlockingSum
    getCurrentSlotInaccurate = getCurrentSlotInaccurateSum
    currentTimeSlotting = currentTimeSlottingSum

instance HasConfiguration => MonadGState (RealMode ssc) where
    gsAdoptedBVData = gsAdoptedBVDataDefault

instance HasConfiguration => MonadDBRead (RealMode ssc) where
    dbGet = dbGetDefault
    dbIterSource = dbIterSourceDefault

instance HasConfiguration => MonadDB (RealMode ssc) where
    dbPut = dbPutDefault
    dbWriteBatch = dbWriteBatchDefault
    dbDelete = dbDeleteDefault

instance MonadBListener (RealMode ssc) where
    onApplyBlocks = onApplyBlocksStub
    onRollbackBlocks = onRollbackBlocksStub

instance
    (HasConfiguration, SscHelpersClass ssc) =>
    MonadBlockDBGeneric (BlockHeader ssc) (Block ssc) Undo (RealMode ssc)
  where
    dbGetBlock  = dbGetBlockDefault @ssc
    dbGetUndo   = dbGetUndoDefault @ssc
    dbGetHeader = dbGetHeaderDefault @ssc

instance
    (HasConfiguration, SscHelpersClass ssc) =>
    MonadBlockDBGeneric (Some IsHeader) (SscBlock ssc) () (RealMode ssc)
  where
    dbGetBlock  = dbGetBlockSscDefault @ssc
    dbGetUndo   = dbGetUndoSscDefault @ssc
    dbGetHeader = dbGetHeaderSscDefault @ssc

instance (HasConfiguration, SscHelpersClass ssc) =>
         MonadBlockDBGenericWrite (BlockHeader ssc) (Block ssc) Undo (RealMode ssc) where
    dbPutBlund = dbPutBlundDefault

instance MonadKnownPeers (RealMode ssc) where
    updatePeersBucket = OQ.Reader.updatePeersBucketReader rmcOutboundQ

instance MonadFormatPeers (RealMode scc) where
    formatKnownPeers = OQ.Reader.formatKnownPeersReader rmcOutboundQ
