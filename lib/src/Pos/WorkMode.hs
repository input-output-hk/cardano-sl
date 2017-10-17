{-# LANGUAGE CPP           #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS -fno-warn-unused-top-binds #-} -- for lenses

module Pos.WorkMode
       ( WorkMode
       , MinWorkMode

       -- * Actual modes
       , RealMode
       , RealModeContext(..)
       , EmptyMempoolExt

       , OQ
       , EnqueuedConversation (..)
       ) where

import           Universum

import           Control.Lens            (makeLensesWith)
import qualified Control.Monad.Reader    as Mtl
import           Ether.Internal          (HasLens (..))
import           Mockable                (Production)
import           System.Wlog             (HasLoggerName (..), LoggerName)

import           Pos.Block.BListener     (MonadBListener (..), onApplyBlocksStub,
                                          onRollbackBlocksStub)
import           Pos.Block.Core          (Block, BlockHeader)
import           Pos.Block.Slog.Types    (HasSlogContext (..), HasSlogGState (..))
import           Pos.Block.Types         (Undo)
import           Pos.Context             (HasNodeContext (..), HasPrimaryKey (..),
                                          HasSscContext (..), NodeContext)
import           Pos.Core                (HasConfiguration, IsHeader)
import           Pos.DB                  (MonadGState (..), NodeDBs)
import           Pos.DB.Block            (dbGetBlockDefault, dbGetBlockSscDefault,
                                          dbGetHeaderDefault, dbGetHeaderSscDefault,
                                          dbGetUndoDefault, dbGetUndoSscDefault,
                                          dbPutBlundDefault)
import           Pos.DB.Class            (MonadBlockDBGeneric (..),
                                          MonadBlockDBGenericWrite (..), MonadDB (..),
                                          MonadDBRead (..))
import           Pos.DB.DB               (gsAdoptedBVDataDefault)
import           Pos.DB.Rocks            (dbDeleteDefault, dbGetDefault,
                                          dbIterSourceDefault, dbPutDefault,
                                          dbWriteBatchDefault)
import           Pos.Delegation.Class    (DelegationVar)
import           Pos.DHT.Real.Types      (KademliaDHTInstance)
import           Pos.Infra.Configuration (HasInfraConfiguration)
import           Pos.KnownPeers          (MonadFormatPeers (..), MonadKnownPeers (..))
import           Pos.Network.Types       (HasNodeType (..), getNodeTypeDefault)
import           Pos.Reporting           (HasReportingContext (..))
import           Pos.Shutdown            (HasShutdownContext (..))
import           Pos.Slotting.Class      (MonadSlots (..))
import           Pos.Slotting.Impl.Sum   (currentTimeSlottingSum,
                                          getCurrentSlotBlockingSum,
                                          getCurrentSlotInaccurateSum, getCurrentSlotSum)
import           Pos.Slotting.MemState   (HasSlottingVar (..), MonadSlotsData)
import           Pos.Ssc.Class.Helpers   (SscHelpersClass)
import           Pos.Ssc.Class.Types     (SscBlock)
import           Pos.Ssc.Extra           (SscMemTag, SscState)
import           Pos.Txp                 (GenericTxpLocalData, MempoolExt,
                                          MonadTxpLocal (..), TxpHolderTag, txNormalize,
                                          txProcessTransaction)
import           Pos.Util                (Some (..))
import           Pos.Util.CompileInfo    (HasCompileInfo)
import           Pos.Util.JsonLog        (HasJsonLogConfig (..), JsonLogConfig,
                                          jsonLogDefault)
import           Pos.Util.LoggerName     (HasLoggerName' (..), getLoggerNameDefault,
                                          modifyLoggerNameDefault)
import           Pos.Util.OutboundQueue  (EnqueuedConversation (..), OQ)
import qualified Pos.Util.OutboundQueue  as OQ.Reader
import           Pos.Util.TimeWarp       (CanJsonLog (..))
import           Pos.Util.UserSecret     (HasUserSecret (..))
import           Pos.Util.Util           (postfixLFields)
import           Pos.WorkMode.Class      (MinWorkMode, WorkMode)
import           Pos.Ssc.GodTossing.Type (SscGodTossing)


data RealModeContext ssc ext = RealModeContext
    { rmcNodeDBs       :: !NodeDBs
    , rmcSscState      :: !(SscState ssc)
    , rmcTxpLocalData  :: !(GenericTxpLocalData ext)
    , rmcDelegationVar :: !DelegationVar
    , rmcJsonLogConfig :: !JsonLogConfig
    , rmcLoggerName    :: !LoggerName
    , rmcNodeContext   :: !(NodeContext ssc)
    , rmcOutboundQ     :: !(OQ (RealMode ssc ext))
    }

type EmptyMempoolExt = ()

type RealMode ssc ext = Mtl.ReaderT (RealModeContext ssc ext) Production

makeLensesWith postfixLFields ''RealModeContext

instance HasLens NodeDBs (RealModeContext ssc ext) NodeDBs where
    lensOf = rmcNodeDBs_L

instance HasLens NodeContext (RealModeContext ssc ext) (NodeContext ssc) where
    lensOf = rmcNodeContext_L

instance HasLens SscMemTag (RealModeContext ssc ext) (SscState ssc) where
    lensOf = rmcSscState_L

instance HasLens TxpHolderTag (RealModeContext ssc ext) (GenericTxpLocalData ext) where
    lensOf = rmcTxpLocalData_L

instance HasLens DelegationVar (RealModeContext ssc ext) DelegationVar where
    lensOf = rmcDelegationVar_L

instance HasNodeType (RealModeContext ssc ext) where
    getNodeType = getNodeTypeDefault @KademliaDHTInstance

instance {-# OVERLAPPABLE #-}
    HasLens tag (NodeContext ssc) r =>
    HasLens tag (RealModeContext ssc ext) r
  where
    lensOf = rmcNodeContext_L . lensOf @tag

instance HasSscContext ssc (RealModeContext ssc ext) where
    sscContext = rmcNodeContext_L . sscContext

instance HasPrimaryKey (RealModeContext ssc ext) where
    primaryKey = rmcNodeContext_L . primaryKey

instance HasReportingContext (RealModeContext ssc ext) where
    reportingContext = rmcNodeContext_L . reportingContext

instance HasUserSecret (RealModeContext ssc ext) where
    userSecret = rmcNodeContext_L . userSecret

instance HasShutdownContext (RealModeContext ssc ext) where
    shutdownContext = rmcNodeContext_L . shutdownContext

instance HasSlottingVar (RealModeContext ssc ext) where
    slottingTimestamp = rmcNodeContext_L . slottingTimestamp
    slottingVar = rmcNodeContext_L . slottingVar

instance HasSlogContext (RealModeContext ssc ext) where
    slogContext = rmcNodeContext_L . slogContext

instance HasSlogGState (RealModeContext ssc ext) where
    slogGState = slogContext . scGState

instance HasNodeContext ssc (RealModeContext ssc ext) where
    nodeContext = rmcNodeContext_L

instance HasLoggerName' (RealModeContext ssc ext) where
    loggerName = rmcLoggerName_L

instance HasJsonLogConfig (RealModeContext ssc ext) where
    jsonLogConfig = rmcJsonLogConfig_L

instance {-# OVERLAPPING #-} HasLoggerName (RealMode ssc ext) where
    getLoggerName = getLoggerNameDefault
    modifyLoggerName = modifyLoggerNameDefault

instance {-# OVERLAPPING #-} CanJsonLog (RealMode ssc ext) where
    jsonLog = jsonLogDefault

instance (HasConfiguration, HasInfraConfiguration, MonadSlotsData ctx (RealMode ssc ext))
      => MonadSlots ctx (RealMode ssc ext)
  where
    getCurrentSlot = getCurrentSlotSum
    getCurrentSlotBlocking = getCurrentSlotBlockingSum
    getCurrentSlotInaccurate = getCurrentSlotInaccurateSum
    currentTimeSlotting = currentTimeSlottingSum

instance HasConfiguration => MonadGState (RealMode ssc ext) where
    gsAdoptedBVData = gsAdoptedBVDataDefault

instance HasConfiguration => MonadDBRead (RealMode ssc ext) where
    dbGet = dbGetDefault
    dbIterSource = dbIterSourceDefault

instance HasConfiguration => MonadDB (RealMode ssc ext) where
    dbPut = dbPutDefault
    dbWriteBatch = dbWriteBatchDefault
    dbDelete = dbDeleteDefault

instance MonadBListener (RealMode ssc ext) where
    onApplyBlocks = onApplyBlocksStub
    onRollbackBlocks = onRollbackBlocksStub

instance
    (HasConfiguration, SscHelpersClass ssc) =>
    MonadBlockDBGeneric BlockHeader Block Undo (RealMode ssc ext)
  where
    dbGetBlock  = dbGetBlockDefault
    dbGetUndo   = dbGetUndoDefault
    dbGetHeader = dbGetHeaderDefault

instance
    HasConfiguration =>
    MonadBlockDBGeneric (Some IsHeader) (SscBlock SscGodTossing) () (RealMode SscGodTossing ext)
  where
    dbGetBlock  = dbGetBlockSscDefault
    dbGetUndo   = dbGetUndoSscDefault
    dbGetHeader = dbGetHeaderSscDefault

instance
    HasConfiguration =>
    MonadBlockDBGenericWrite BlockHeader Block Undo (RealMode SscGodTossing ext)
  where
    dbPutBlund = dbPutBlundDefault

instance MonadKnownPeers (RealMode ssc ext) where
    updatePeersBucket = OQ.Reader.updatePeersBucketReader rmcOutboundQ

instance MonadFormatPeers (RealMode scc ext) where
    formatKnownPeers = OQ.Reader.formatKnownPeersReader rmcOutboundQ

type instance MempoolExt (RealMode ssc ext) = ext

instance (HasConfiguration, HasInfraConfiguration, HasCompileInfo) =>
         MonadTxpLocal (RealMode ssc ()) where
    txpNormalize = txNormalize
    txpProcessTx = txProcessTransaction
