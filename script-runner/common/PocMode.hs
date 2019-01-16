{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

-- | Execution mode used in Auxx.

module PocMode
       (
       -- * Mode, context, etc.
         AuxxContext (..)
       , PocMode
       , MonadPocMode
       , acScriptOptions
       , acTopology
       , acStatePath
       , ScriptBuilder(..)
       , CompiledScript(..)
       , SlotTrigger(..)
       , Script(..)
       , ScriptT(runScriptT)
       , InputParams(..)
       , InputParams2(..)
       , ScriptParams(..)

       -- * Helpers
       , realModeToAuxx
       , writeBrickChan
       , nodeHandles
       ) where

import           Universum

import           Brick.BChan (BChan, writeBChan)
import           Control.Lens (lens, makeLenses)
import           Control.Monad.Reader (withReaderT)
import           Control.Monad.Trans.Resource (transResourceT)
import           Data.Conduit (transPipe)
import           Data.Constraint (Dict)
import           Data.Default (Default (def))
import qualified Data.Map as Map
import           Prelude (show)

import           BrickUITypes (CustomEvent)

import           Pos.Chain.Block (HasSlogContext (slogContext),
                     HasSlogGState (slogGState))
import           Pos.Chain.Genesis as Genesis (Config)
import           Pos.Chain.Ssc (HasSscContext (sscContext))
import           Pos.Client.KeyStorage (MonadKeys (modifySecret),
                     MonadKeysRead (getSecret), getSecretDefault,
                     modifySecretDefault)
import           Pos.Context (HasNodeContext (nodeContext))
import           Pos.Core (SlotCount, SlotId)
import           Pos.Core (HasPrimaryKey (primaryKey))
import           Pos.Core.JsonLog (CanJsonLog (jsonLog))
import           Pos.Core.Reporting (HasMisbehaviorMetrics (misbehaviorMetrics),
                     MonadReporting (report))
import           Pos.Core.Slotting
                     (HasSlottingVar (slottingTimestamp, slottingVar),
                     MonadSlotsData)
import           Pos.DB (MonadGState (gsAdoptedBVData))
import           Pos.DB.Block (MonadBListener (onApplyBlocks, onRollbackBlocks))
import           Pos.DB.Class
                     (MonadDB (dbDelete, dbPut, dbPutSerBlunds, dbWriteBatch),
                     MonadDBRead (dbGet, dbGetSerBlock, dbGetSerBlund, dbGetSerUndo, dbIterSource))
import           Pos.DB.Txp (MempoolExt,
                     MonadTxpLocal (txpNormalize, txpProcessTx), txNormalize,
                     txProcessTransaction)
import           Pos.Infra.Diffusion.Types (Diffusion)
import           Pos.Infra.Network.Types (HasNodeType (getNodeType),
                     NodeType (NodeEdge))
import           Pos.Infra.Network.Yaml (Topology)
import           Pos.Infra.Shutdown (HasShutdownContext (shutdownContext))
import           Pos.Infra.Slotting.Class (MonadSlots (currentTimeSlotting, getCurrentSlot, getCurrentSlotBlocking, getCurrentSlotInaccurate))
import           Pos.Launcher (HasConfigurations)
import           Pos.Util (HasLens (lensOf))
import           Pos.Util.CompileInfo (HasCompileInfo)
import           Pos.Util.LoggerName (HasLoggerName' (loggerName))
import           Pos.Util.UserSecret (HasUserSecret (userSecret))
import           Pos.Util.Wlog (HasLoggerName (askLoggerName, modifyLoggerName))
import           Pos.WorkMode (EmptyMempoolExt, RealMode, RealModeContext)

import           BrickUITypes
import           Types (NodeHandle, NodeType, ScriptRunnerOptions, Todo)

type PocMode = ReaderT AuxxContext IO

class (m ~ PocMode, HasConfigurations, HasCompileInfo) => MonadPocMode m
instance (HasConfigurations, HasCompileInfo) => MonadPocMode PocMode

data AuxxContext = AuxxContext
    { _acRealModeContext :: !(RealModeContext EmptyMempoolExt)
    , _acEventChan       :: !(BChan CustomEvent)
    , _acNodeHandles     :: !(TVar (Map (Types.NodeType,Integer) NodeHandle))
    , _acScriptOptions   :: !(ScriptRunnerOptions)
    , _acTopology        :: !(Topology)
    , _acStatePath       :: !(Text)
    }

makeLenses ''AuxxContext

data SlotTrigger = SlotTrigger (Dict HasConfigurations -> Diffusion PocMode -> PocMode ())
instance Show SlotTrigger where
  show _ = "IO ()"

data CompiledScript = CompiledScript
  { slotTriggers   :: Map.Map SlotId SlotTrigger
  , startupActions :: [ SlotTrigger ]
  } deriving (Show, Generic)
instance Default CompiledScript where def = CompiledScript def def

data ScriptBuilder = ScriptBuilder
  { sbScript        :: CompiledScript
  , sbEpochSlots    :: SlotCount
  , sbGenesisConfig :: Config
  }

data ScriptParams = ScriptParams
  { spScript            :: !(Script ())
  , spTodo              :: Todo
  , spRecentSystemStart :: Bool
  , spStartCoreAndRelay :: Bool
  }

data InputParams = InputParams
  { ipEventChan    :: BChan CustomEvent
  , ipReplyChan    :: BChan Reply
  , ipScriptParams :: ScriptParams
  , ipStatePath    :: Text
  }
data InputParams2 = InputParams2
  { ip2EventChan    :: BChan CustomEvent
  , ip2ReplyChan    :: BChan Reply
  , ip2ScriptParams :: ScriptParams
  , ip2StatePath    :: Text
  }

newtype ScriptT m a = ScriptT { runScriptT :: StateT ScriptBuilder m a } deriving (Functor, Applicative, Monad, MonadState ScriptBuilder)
newtype Script a = Script { runScriptMonad :: ScriptT (Identity) a } deriving (Applicative, Functor, Monad, MonadState ScriptBuilder)

writeBrickChan :: CustomEvent -> PocMode ()
writeBrickChan event = do
  chan <- view acEventChan
  liftIO $ writeBChan chan event

nodeHandles :: PocMode (TVar (Map (Types.NodeType,Integer) NodeHandle))
nodeHandles = view acNodeHandles

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

-- | Turn 'RealMode' action into 'PocMode' action.
realModeToAuxx :: RealMode EmptyMempoolExt a -> PocMode a
realModeToAuxx = withReaderT _acRealModeContext

----------------------------------------------------------------------------
-- Boilerplate instances
----------------------------------------------------------------------------

instance HasSscContext AuxxContext where
    sscContext = acRealModeContext . sscContext

instance HasPrimaryKey AuxxContext where
    primaryKey = acRealModeContext . primaryKey

-- | Ignore reports.
-- FIXME it's a bad sign that we even need this instance.
-- The pieces of the software which the block generator uses should never
-- even try to report.
instance MonadReporting PocMode where
    report _ = pure ()

-- | Ignore reports.
-- FIXME it's a bad sign that we even need this instance.
instance HasMisbehaviorMetrics AuxxContext where
    misbehaviorMetrics = lens (const Nothing) const

instance HasUserSecret AuxxContext where
    userSecret = acRealModeContext . userSecret

instance HasShutdownContext AuxxContext where
    shutdownContext = acRealModeContext . shutdownContext

instance HasNodeContext AuxxContext where
    nodeContext = acRealModeContext . nodeContext

instance HasSlottingVar AuxxContext where
    slottingTimestamp = acRealModeContext . slottingTimestamp
    slottingVar = acRealModeContext . slottingVar

instance HasNodeType AuxxContext where
    getNodeType _ = NodeEdge

instance {-# OVERLAPPABLE #-}
    HasLens tag (RealModeContext EmptyMempoolExt) r =>
    HasLens tag AuxxContext r
  where
    lensOf = acRealModeContext . lensOf @tag

instance HasLoggerName' AuxxContext where
    loggerName = acRealModeContext . loggerName

instance HasSlogContext AuxxContext where
    slogContext = acRealModeContext . slogContext

instance HasSlogGState AuxxContext where
    slogGState = acRealModeContext . slogGState

instance MonadSlotsData ctx PocMode => MonadSlots ctx PocMode where
    getCurrentSlot = realModeToAuxx . getCurrentSlot
    getCurrentSlotBlocking = realModeToAuxx . getCurrentSlotBlocking
    getCurrentSlotInaccurate = realModeToAuxx . getCurrentSlotInaccurate
    currentTimeSlotting = realModeToAuxx currentTimeSlotting

instance {-# OVERLAPPING #-} HasLoggerName PocMode where
    askLoggerName = realModeToAuxx askLoggerName
    modifyLoggerName f action = do
        auxxCtx <- ask
        let auxxToRealMode :: PocMode a -> RealMode EmptyMempoolExt a
            auxxToRealMode = withReaderT (\realCtx -> set acRealModeContext realCtx auxxCtx)
        realModeToAuxx $ modifyLoggerName f $ auxxToRealMode action

instance {-# OVERLAPPING #-} CanJsonLog PocMode where
    jsonLog = realModeToAuxx ... jsonLog

instance MonadDBRead PocMode where
    dbGet = realModeToAuxx ... dbGet
    dbIterSource tag p =
        transPipe (transResourceT realModeToAuxx) (dbIterSource tag p)
    dbGetSerBlock = realModeToAuxx ... dbGetSerBlock
    dbGetSerUndo = realModeToAuxx ... dbGetSerUndo
    dbGetSerBlund = realModeToAuxx ... dbGetSerBlund

instance MonadDB PocMode where
    dbPut = realModeToAuxx ... dbPut
    dbWriteBatch = realModeToAuxx ... dbWriteBatch
    dbDelete = realModeToAuxx ... dbDelete
    dbPutSerBlunds = realModeToAuxx ... dbPutSerBlunds

instance MonadGState PocMode where
    gsAdoptedBVData = realModeToAuxx ... gsAdoptedBVData

instance MonadBListener PocMode where
    onApplyBlocks = realModeToAuxx ... onApplyBlocks
    onRollbackBlocks = realModeToAuxx ... onRollbackBlocks

instance MonadKeysRead PocMode where
    getSecret = getSecretDefault

instance MonadKeys PocMode where
    modifySecret = modifySecretDefault

type instance MempoolExt PocMode = EmptyMempoolExt

instance MonadTxpLocal PocMode where
    txpNormalize pm = withReaderT _acRealModeContext . txNormalize pm
    txpProcessTx genesisConfig txpConfig = withReaderT _acRealModeContext . txProcessTransaction genesisConfig txpConfig

