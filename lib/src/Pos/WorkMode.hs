{-# LANGUAGE CPP           #-}
{-# LANGUAGE RankNTypes    #-}
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
       ) where

import           Universum

import           Control.Lens (makeLensesWith)
import qualified Control.Monad.Reader as Mtl
import           Mockable (Production)
import           System.Wlog (HasLoggerName (..), LoggerName)

import           Pos.Block.BListener (MonadBListener (..), onApplyBlocksStub, onRollbackBlocksStub)
import           Pos.Block.Slog (HasSlogContext (..), HasSlogGState (..))
import           Pos.Context (HasNodeContext (..), HasPrimaryKey (..), HasSscContext (..),
                              NodeContext)
import           Pos.Core (HasConfiguration)
import           Pos.DB (MonadGState (..), NodeDBs)
import           Pos.DB.Block (dbGetSerBlockRealDefault, dbGetSerUndoRealDefault,
                               dbPutSerBlundsRealDefault)
import           Pos.DB.Class (MonadDB (..), MonadDBRead (..))
import           Pos.DB.DB (gsAdoptedBVDataDefault)
import           Pos.DB.Rocks (dbDeleteDefault, dbGetDefault, dbIterSourceDefault, dbPutDefault,
                               dbWriteBatchDefault)
import           Pos.Delegation.Class (DelegationVar)
import           Pos.DHT.Real.Param (KademliaParams)
import           Pos.Network.Types (HasNodeType (..), getNodeTypeDefault)
import           Pos.Reporting (HasMisbehaviorMetrics (..), MonadReporting (..), Reporter (..))
import           Pos.Shutdown (HasShutdownContext (..))
import           Pos.Slotting.Class (MonadSlots (..))
import           Pos.Slotting.Impl (currentTimeSlottingSimple,
                                    getCurrentSlotBlockingSimple,
                                    getCurrentSlotInaccurateSimple, getCurrentSlotSimple)
import           Pos.Slotting.MemState (HasSlottingVar (..), MonadSlotsData)
import           Pos.Ssc.Mem (SscMemTag)
import           Pos.Ssc.Types (SscState)
import           Pos.Txp (GenericTxpLocalData, HasTxpConfiguration, MempoolExt, MonadTxpLocal (..),
                          TxpHolderTag, txNormalize, txProcessTransaction)
import           Pos.Util.CompileInfo (HasCompileInfo)
import           Pos.Util.JsonLog.Events (HasJsonLogConfig (..), JsonLogConfig, jsonLogDefault)
import           Pos.Util.Lens (postfixLFields)
import           Pos.Util.LoggerName (HasLoggerName' (..), askLoggerNameDefault,
                                      modifyLoggerNameDefault)
import           Pos.Util.TimeWarp (CanJsonLog (..))
import           Pos.Util.UserSecret (HasUserSecret (..))
import           Pos.Util.Util (HasLens (..))
import           Pos.WorkMode.Class (MinWorkMode, WorkMode)

data RealModeContext ext = RealModeContext
    { rmcNodeDBs       :: !NodeDBs
    , rmcSscState      :: !SscState
    , rmcTxpLocalData  :: !(GenericTxpLocalData ext)
    , rmcDelegationVar :: !DelegationVar
    , rmcJsonLogConfig :: !JsonLogConfig
    , rmcLoggerName    :: !LoggerName
    , rmcNodeContext   :: !NodeContext
    , rmcReporter      :: !(Reporter IO)
      -- ^ How to do reporting. It's in here so that we can have
      -- 'MonadReporting (RealMode ext)' in the mean-time, until we
      -- re-architecht the reporting system so that it's not built-in to the
      -- application's monad.
    }

type EmptyMempoolExt = ()

type RealMode ext = Mtl.ReaderT (RealModeContext ext) Production

makeLensesWith postfixLFields ''RealModeContext

instance HasLens NodeDBs (RealModeContext ext) NodeDBs where
    lensOf = rmcNodeDBs_L

instance HasLens NodeContext (RealModeContext ext) NodeContext where
    lensOf = rmcNodeContext_L

instance HasLens SscMemTag (RealModeContext ext) SscState where
    lensOf = rmcSscState_L

instance HasLens TxpHolderTag (RealModeContext ext) (GenericTxpLocalData ext) where
    lensOf = rmcTxpLocalData_L

instance HasLens DelegationVar (RealModeContext ext) DelegationVar where
    lensOf = rmcDelegationVar_L

instance HasNodeType (RealModeContext ext) where
    getNodeType = getNodeTypeDefault @KademliaParams

instance {-# OVERLAPPABLE #-}
    HasLens tag NodeContext r =>
    HasLens tag (RealModeContext ext) r
  where
    lensOf = rmcNodeContext_L . lensOf @tag

instance HasSscContext (RealModeContext ext) where
    sscContext = rmcNodeContext_L . sscContext

instance HasPrimaryKey (RealModeContext ext) where
    primaryKey = rmcNodeContext_L . primaryKey

instance HasMisbehaviorMetrics (RealModeContext ext) where
    misbehaviorMetrics = rmcNodeContext_L . misbehaviorMetrics

instance HasUserSecret (RealModeContext ext) where
    userSecret = rmcNodeContext_L . userSecret

instance HasShutdownContext (RealModeContext ext) where
    shutdownContext = rmcNodeContext_L . shutdownContext

instance HasSlottingVar (RealModeContext ext) where
    slottingTimestamp = rmcNodeContext_L . slottingTimestamp
    slottingVar = rmcNodeContext_L . slottingVar

instance HasSlogContext (RealModeContext ext) where
    slogContext = rmcNodeContext_L . slogContext

instance HasSlogGState (RealModeContext ext) where
    slogGState = slogContext . scGState

instance HasNodeContext (RealModeContext ext) where
    nodeContext = rmcNodeContext_L

instance HasLoggerName' (RealModeContext ext) where
    loggerName = rmcLoggerName_L

instance HasJsonLogConfig (RealModeContext ext) where
    jsonLogConfig = rmcJsonLogConfig_L

instance {-# OVERLAPPING #-} HasLoggerName (RealMode ext) where
    askLoggerName = askLoggerNameDefault
    modifyLoggerName = modifyLoggerNameDefault

instance {-# OVERLAPPING #-} CanJsonLog (RealMode ext) where
    jsonLog = jsonLogDefault

instance (HasConfiguration, MonadSlotsData ctx (RealMode ext))
      => MonadSlots ctx (RealMode ext)
  where
    getCurrentSlot = getCurrentSlotSimple
    getCurrentSlotBlocking = getCurrentSlotBlockingSimple
    getCurrentSlotInaccurate = getCurrentSlotInaccurateSimple
    currentTimeSlotting = currentTimeSlottingSimple

instance HasConfiguration => MonadGState (RealMode ext) where
    gsAdoptedBVData = gsAdoptedBVDataDefault

instance HasConfiguration => MonadDBRead (RealMode ext) where
    dbGet = dbGetDefault
    dbIterSource = dbIterSourceDefault
    dbGetSerBlock = dbGetSerBlockRealDefault
    dbGetSerUndo = dbGetSerUndoRealDefault

instance HasConfiguration => MonadDB (RealMode ext) where
    dbPut = dbPutDefault
    dbWriteBatch = dbWriteBatchDefault
    dbDelete = dbDeleteDefault
    dbPutSerBlunds = dbPutSerBlundsRealDefault

instance MonadBListener (RealMode ext) where
    onApplyBlocks = onApplyBlocksStub
    onRollbackBlocks = onRollbackBlocksStub

type instance MempoolExt (RealMode ext) = ext

instance (HasConfiguration, HasTxpConfiguration, HasCompileInfo) =>
         MonadTxpLocal (RealMode ()) where
    txpNormalize = txNormalize
    txpProcessTx = txProcessTransaction

instance MonadReporting (RealMode ext) where
    report rt = Mtl.ask >>= liftIO . flip runReporter rt . rmcReporter
