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
       , isTempDbUsed
       , realModeToAuxx
       , makePubKeyAddressAuxx
       , deriveHDAddressAuxx
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
import           Pos.Client.Txp.Balances          (MonadBalances (..), getBalanceFromUtxo,
                                                   getOwnUtxosGenesis)
import           Pos.Client.Txp.History           (MonadTxHistory (..),
                                                   getBlockHistoryDefault,
                                                   getLocalHistoryDefault, saveTxDefault)
import           Pos.Communication                (NodeId)
import           Pos.Context                      (HasNodeContext (..))
import           Pos.Core                         (Address, HasConfiguration,
                                                   HasPrimaryKey (..),
                                                   IsBootstrapEraAddr (..), IsHeader,
                                                   deriveFirstHDAddress,
                                                   makePubKeyAddress, siEpoch)
import           Pos.Crypto                       (EncryptedSecretKey, PublicKey,
                                                   emptyPassphrase)
import           Pos.DB                           (MonadGState (..), gsIsBootstrapEra)
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
import           Pos.Txp                          (MempoolExt, MonadTxpLocal (..),
                                                   txNormalize, txProcessTransaction)
import           Pos.Txp.DB.Utxo                  (getFilteredUtxo)
import           Pos.Util                         (Some (..))
import           Pos.Util.JsonLog                 (HasJsonLogConfig (..))
import           Pos.Util.LoggerName              (HasLoggerName' (..))
import qualified Pos.Util.OutboundQueue           as OQ.Reader
import           Pos.Util.TimeWarp                (CanJsonLog (..))
import           Pos.Util.UserSecret              (HasUserSecret (..))
import           Pos.Util.Util                    (HasLens (..), postfixLFields)
import           Pos.WorkMode                     (EmptyMempoolExt, RealMode,
                                                   RealModeContext (..))

-- | Command execution context.
data CmdCtx = CmdCtx
    { ccPeers :: ![NodeId]
    }

type AuxxSscType = SscGodTossing

type AuxxMode = ReaderT AuxxContext Production

data AuxxContext = AuxxContext
    { acRealModeContext :: !(RealModeContext AuxxSscType EmptyMempoolExt)
    , acCmdCtx          :: !CmdCtx
    , acTempDbUsed      :: !Bool
    }

makeLensesWith postfixLFields ''AuxxContext

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

-- | Get 'CmdCtx' in 'AuxxMode'.
getCmdCtx :: AuxxMode CmdCtx
getCmdCtx = view acCmdCtx_L

isTempDbUsed :: AuxxMode Bool
isTempDbUsed = view acTempDbUsed_L

-- | Turn 'RealMode' action into 'AuxxMode' action.
realModeToAuxx :: RealMode AuxxSscType EmptyMempoolExt a -> AuxxMode a
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
    HasLens tag (RealModeContext AuxxSscType EmptyMempoolExt) r =>
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
        auxxCtx <- ask
        let auxxToRealMode :: AuxxMode a -> RealMode AuxxSscType EmptyMempoolExt a
            auxxToRealMode = withReaderT (\realCtx -> set acRealModeContext_L realCtx auxxCtx)
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

instance HasConfiguration => MonadBalances AuxxMode where
    getOwnUtxos addrs = ifM isTempDbUsed (getOwnUtxosGenesis addrs) (getFilteredUtxo addrs)
    getBalance = getBalanceFromUtxo

instance (HasConfiguration, HasInfraConfiguration, HasGtConfiguration) =>
         MonadTxHistory AuxxSscType AuxxMode where
    getBlockHistory = getBlockHistoryDefault @AuxxSscType
    getLocalHistory = getLocalHistoryDefault
    saveTx = saveTxDefault

instance MonadKnownPeers AuxxMode where
    updatePeersBucket = realModeToAuxx ... updatePeersBucket

instance MonadFormatPeers AuxxMode where
    formatKnownPeers = OQ.Reader.formatKnownPeersReader (rmcOutboundQ . acRealModeContext)

instance (HasConfiguration, HasInfraConfiguration) => MonadAddresses AuxxMode where
    type AddrData AuxxMode = PublicKey
    getNewAddress = makePubKeyAddressAuxx

type instance MempoolExt AuxxMode = EmptyMempoolExt

instance (HasConfiguration, HasInfraConfiguration) => MonadTxpLocal AuxxMode where
    txpNormalize = withReaderT acRealModeContext txNormalize
    txpProcessTx = withReaderT acRealModeContext . txProcessTransaction

-- | In order to create an 'Address' from a 'PublicKey' we need to
-- choose suitable stake distribution. We want to pick it based on
-- whether we are currently in bootstrap era.
makePubKeyAddressAuxx :: (HasConfiguration, HasInfraConfiguration) => PublicKey -> AuxxMode Address
makePubKeyAddressAuxx pk = do
    epochIndex <- siEpoch <$> getCurrentSlotInaccurate
    ibea <- IsBootstrapEraAddr <$> gsIsBootstrapEra epochIndex
    pure $ makePubKeyAddress ibea pk

-- | Similar to @makePubKeyAddressAuxx@ but create HD address.
deriveHDAddressAuxx :: (HasConfiguration, HasInfraConfiguration) => EncryptedSecretKey -> AuxxMode Address
deriveHDAddressAuxx hdwSk = do
    epochIndex <- siEpoch <$> getCurrentSlotInaccurate
    ibea <- IsBootstrapEraAddr <$> gsIsBootstrapEra epochIndex
    pure $ fst $ fromMaybe (error "makePubKeyHDAddressAuxx: pass mismatch") $
        deriveFirstHDAddress ibea emptyPassphrase hdwSk
