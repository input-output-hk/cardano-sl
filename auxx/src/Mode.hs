{-# LANGUAGE TypeFamilies #-}

-- | Execution mode used in Auxx.

module Mode
       (
         -- * Extra types
         CmdCtx (..)

       -- * Mode, context, etc.
       , AuxxContext (..)
       , AuxxMode
       , AuxxSscType

       -- * Helpers
       , getCmdCtx
       , realModeToAuxx

       -- * Specialisations of utils
       , getOwnUtxos
       , getBalance
       ) where

import           Universum

import           Control.Lens                     (makeLensesWith)
import           Control.Monad.Morph              (hoist)
import           Control.Monad.Reader             (withReaderT)
import           Mockable                         (Production)
import           System.Wlog                      (HasLoggerName (..))

import           Pos.Block.BListener              (MonadBListener (..))
import           Pos.Block.Core                   (Block, BlockHeader)
import           Pos.Block.Slog                   (HasSlogContext (..),
                                                   HasSlogGState (..))
import           Pos.Block.Types                  (Undo)
import           Pos.Client.Txp.Addresses         (MonadAddresses (..))
import           Pos.Client.Txp.Balances          (getBalanceFromUtxo)
import           Pos.Client.Txp.History           (MonadTxHistory (..),
                                                   getBlockHistoryDefault,
                                                   getLocalHistoryDefault, saveTxDefault)
import           Pos.Communication                (NodeId)
import           Pos.Context                      (HasNodeContext (..), unGenesisUtxo)
import           Pos.Core                         (Address, Coin, HasConfiguration,
                                                   HasPrimaryKey (..), IsHeader)
import           Pos.Crypto                       (PublicKey)
import           Pos.DB                           (MonadGState (..))
import           Pos.DB.Class                     (MonadBlockDBGeneric (..),
                                                   MonadBlockDBGenericWrite (..),
                                                   MonadDB (..), MonadDBRead (..))
import           Pos.Infra.Configuration          (HasInfraConfiguration)
import           Pos.KnownPeers                   (MonadFormatPeers (..),
                                                   MonadKnownPeers (..))
import           Pos.Network.Types                (HasNodeType (..), NodeType (..))
import           Pos.Reporting                    (HasReportingContext (..))
import           Pos.Shutdown                     (HasShutdownContext (..))
import           Pos.Slotting.Class               (MonadSlots (..))
import           Pos.Slotting.MemState            (HasSlottingVar (..), MonadSlotsData)
import           Pos.Ssc.Class                    (HasSscContext (..), SscBlock)
import           Pos.Ssc.GodTossing               (SscGodTossing)
import           Pos.Ssc.GodTossing.Configuration (HasGtConfiguration)
import           Pos.Txp                          (Utxo, filterUtxoByAddrs, genesisUtxo)
import           Pos.Util                         (Some (..))
import           Pos.Util.JsonLog                 (HasJsonLogConfig (..))
import           Pos.Util.LoggerName              (HasLoggerName' (..))
import qualified Pos.Util.OutboundQueue           as OQ.Reader
import           Pos.Util.TimeWarp                (CanJsonLog (..))
import           Pos.Util.UserSecret              (HasUserSecret (..))
import           Pos.Util.Util                    (HasLens (..), postfixLFields)
import           Pos.WorkMode                     (RealMode, RealModeContext (..))

import           Pos.Auxx.Hacks                   (makePubKeyAddressAuxx)

-- | Command execution context.
data CmdCtx = CmdCtx
    { ccPeers :: ![NodeId]
    }

type AuxxSscType = SscGodTossing

type AuxxMode = ReaderT AuxxContext Production

data AuxxContext = AuxxContext
    { acRealModeContext :: !(RealModeContext AuxxSscType)
    , acCmdCtx          :: !CmdCtx
    }

makeLensesWith postfixLFields ''AuxxContext

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

-- | Get 'CmdCtx' in 'AuxxMode'.
getCmdCtx :: AuxxMode CmdCtx
getCmdCtx = view acCmdCtx_L

-- | Turn 'RealMode' action into 'AuxxMode' action.
realModeToAuxx :: RealMode AuxxSscType a -> AuxxMode a
realModeToAuxx = withReaderT acRealModeContext

----------------------------------------------------------------------------
-- Boilerplate instances
----------------------------------------------------------------------------

instance HasSscContext AuxxSscType AuxxContext where
    sscContext = acRealModeContext_L . sscContext

instance HasPrimaryKey AuxxContext where
    primaryKey = acRealModeContext_L . primaryKey

instance HasReportingContext AuxxContext  where
    reportingContext = acRealModeContext_L . reportingContext

instance HasUserSecret AuxxContext where
    userSecret = acRealModeContext_L . userSecret

instance HasShutdownContext AuxxContext where
    shutdownContext = acRealModeContext_L . shutdownContext

instance HasNodeContext AuxxSscType AuxxContext where
    nodeContext = acRealModeContext_L . nodeContext

instance HasSlottingVar AuxxContext where
    slottingTimestamp = acRealModeContext_L . slottingTimestamp
    slottingVar = acRealModeContext_L . slottingVar

instance HasNodeType AuxxContext where
    getNodeType _ = NodeEdge

instance {-# OVERLAPPABLE #-}
    HasLens tag (RealModeContext AuxxSscType) r =>
    HasLens tag AuxxContext r
  where
    lensOf = acRealModeContext_L . lensOf @tag

instance HasLoggerName' AuxxContext where
    loggerName = acRealModeContext_L . loggerName

instance HasSlogContext AuxxContext where
    slogContext = acRealModeContext_L . slogContext

instance HasSlogGState AuxxContext where
    slogGState = acRealModeContext_L . slogGState

instance HasJsonLogConfig AuxxContext where
    jsonLogConfig = acRealModeContext_L . jsonLogConfig

instance (HasConfiguration, HasInfraConfiguration, MonadSlotsData ctx AuxxMode)
      => MonadSlots ctx AuxxMode
  where
    getCurrentSlot = realModeToAuxx getCurrentSlot
    getCurrentSlotBlocking = realModeToAuxx getCurrentSlotBlocking
    getCurrentSlotInaccurate = realModeToAuxx getCurrentSlotInaccurate
    currentTimeSlotting = realModeToAuxx currentTimeSlotting

instance {-# OVERLAPPING #-} HasLoggerName AuxxMode where
    getLoggerName = realModeToAuxx getLoggerName
    modifyLoggerName f action = do
        cmdCtx <- getCmdCtx
        let auxxToRealMode :: AuxxMode a -> RealMode AuxxSscType a
            auxxToRealMode = withReaderT (flip AuxxContext cmdCtx)
        realModeToAuxx $ modifyLoggerName f $ auxxToRealMode action

instance {-# OVERLAPPING #-} CanJsonLog AuxxMode where
    jsonLog = realModeToAuxx ... jsonLog

instance HasConfiguration => MonadDBRead AuxxMode where
    dbGet = realModeToAuxx ... dbGet
    dbIterSource tag p = hoist (hoist realModeToAuxx) (dbIterSource tag p)

instance HasConfiguration => MonadDB AuxxMode where
    dbPut = realModeToAuxx ... dbPut
    dbWriteBatch = realModeToAuxx ... dbWriteBatch
    dbDelete = realModeToAuxx ... dbDelete

instance (HasConfiguration, HasGtConfiguration) =>
         MonadBlockDBGenericWrite (BlockHeader AuxxSscType) (Block AuxxSscType) Undo AuxxMode where
    dbPutBlund = realModeToAuxx ... dbPutBlund

instance (HasConfiguration, HasGtConfiguration) =>
         MonadBlockDBGeneric (BlockHeader AuxxSscType) (Block AuxxSscType) Undo AuxxMode
  where
    dbGetBlock  = realModeToAuxx ... dbGetBlock
    dbGetUndo   = realModeToAuxx ... dbGetUndo @(BlockHeader AuxxSscType) @(Block AuxxSscType) @Undo
    dbGetHeader = realModeToAuxx ... dbGetHeader @(BlockHeader AuxxSscType) @(Block AuxxSscType) @Undo

instance (HasConfiguration, HasGtConfiguration) =>
         MonadBlockDBGeneric (Some IsHeader) (SscBlock AuxxSscType) () AuxxMode
  where
    dbGetBlock  = realModeToAuxx ... dbGetBlock
    dbGetUndo   = realModeToAuxx ... dbGetUndo @(Some IsHeader) @(SscBlock AuxxSscType) @()
    dbGetHeader = realModeToAuxx ... dbGetHeader @(Some IsHeader) @(SscBlock AuxxSscType) @()

instance HasConfiguration => MonadGState AuxxMode where
    gsAdoptedBVData = realModeToAuxx ... gsAdoptedBVData

instance HasConfiguration => MonadBListener AuxxMode where
    onApplyBlocks = realModeToAuxx ... onApplyBlocks
    onRollbackBlocks = realModeToAuxx ... onRollbackBlocks

-- FIXME: I preserved the old behavior, but it most likely should be
-- changed!
getOwnUtxos :: (HasConfiguration, Applicative m) => [Address] -> m Utxo
getOwnUtxos addrs = pure $ filterUtxoByAddrs addrs $ unGenesisUtxo genesisUtxo

getBalance :: (HasConfiguration, Applicative m) => Address -> m Coin
getBalance = getBalanceFromUtxo getOwnUtxos

instance (HasConfiguration, HasInfraConfiguration, HasGtConfiguration) =>
         MonadTxHistory AuxxSscType AuxxMode where
    getBlockHistory = getBlockHistoryDefault @AuxxSscType
    getLocalHistory = getLocalHistoryDefault
    saveTx = saveTxDefault

instance MonadKnownPeers AuxxMode where
    updatePeersBucket = realModeToAuxx ... updatePeersBucket

instance MonadFormatPeers AuxxMode where
    formatKnownPeers = OQ.Reader.formatKnownPeersReader (rmcOutboundQ . acRealModeContext)

instance HasConfiguration => MonadAddresses AuxxMode where
    type AddrData AuxxMode = PublicKey
    getNewAddress = makePubKeyAddressAuxx
