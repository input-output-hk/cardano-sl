{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Pos.Wallet.Web.Mode
       ( WalletWebMode
       , WalletWebModeContextTag
       , WalletWebModeContext(..)
       , MonadWalletWebMode
       , MonadWebSockets
       , MonadFullWalletWebMode
       ) where

import           Universum

import           Control.Lens                   (makeLensesWith)
import           Control.Monad.Catch            (MonadMask)
import qualified Control.Monad.Reader           as Mtl
import           Control.Monad.Trans.Control    (MonadBaseControl)
import           Ether.Internal                 (HasLens (..))
import           Mockable                       (Production)
import           System.Wlog                    (HasLoggerName (..))

import           Pos.Block.Core                 (Block, BlockHeader)
import           Pos.Block.Slog                 (HasSlogContext (..), HasSlogGState (..))
import           Pos.Block.Types                (Undo)
import           Pos.Context                    (HasNodeContext (..))
import           Pos.Core                       (HasCoreConstants, HasPrimaryKey (..),
                                                 IsHeader)
import           Pos.Crypto                     (PassPhrase)
import           Pos.DB                         (MonadGState (..))
import           Pos.DB.Block                   (MonadBlockDB, dbGetBlockDefault,
                                                 dbGetBlockSscDefault, dbGetHeaderDefault,
                                                 dbGetHeaderSscDefault, dbGetUndoDefault,
                                                 dbGetUndoSscDefault, dbPutBlundDefault)
import           Pos.DB.Class                   (MonadBlockDBGeneric (..),
                                                 MonadBlockDBGenericWrite (..),
                                                 MonadDB (..), MonadDBRead (..))
import           Pos.DB.DB                      (gsAdoptedBVDataDefault)
import           Pos.DB.Rocks                   (dbDeleteDefault, dbGetDefault,
                                                 dbIterSourceDefault, dbPutDefault,
                                                 dbWriteBatchDefault)

import           Pos.Client.Txp.Addresses       (MonadAddresses (..))
import           Pos.Client.Txp.Balances        (MonadBalances (..), getBalanceDefault,
                                                 getOwnUtxosDefault)
import           Pos.Client.Txp.History         (MonadTxHistory (..),
                                                 getBlockHistoryDefault,
                                                 getLocalHistoryDefault, saveTxDefault)
import           Pos.Genesis                    (GenesisUtxo)
import           Pos.KnownPeers                 (MonadFormatPeers (..),
                                                 MonadKnownPeers (..))
import           Pos.Recovery.Info              (MonadRecoveryInfo)
import           Pos.Reporting                  (HasReportingContext (..))
import           Pos.Shutdown                   (HasShutdownContext (..))
import           Pos.Slotting.Class             (MonadSlots (..))
import           Pos.Slotting.Impl.Sum          (currentTimeSlottingSum,
                                                 getCurrentSlotBlockingSum,
                                                 getCurrentSlotInaccurateSum,
                                                 getCurrentSlotSum)
import           Pos.Slotting.MemState          (HasSlottingVar (..), MonadSlotsData)
import           Pos.Ssc.Class.Helpers          (SscHelpersClass)
import           Pos.Ssc.Class.Types            (HasSscContext (..), SscBlock)
import           Pos.StateLock                  (StateLock)
import           Pos.Txp.MemState               (MonadTxpMem)
import           Pos.Util                       (HasLens', Some (..))
import           Pos.Util.JsonLog               (HasJsonLogConfig (..), jsonLogDefault)
import           Pos.Util.LoggerName            (HasLoggerName' (..),
                                                 getLoggerNameDefault,
                                                 modifyLoggerNameDefault)
import qualified Pos.Util.OutboundQueue         as OQ.Reader
import           Pos.Util.TimeWarp              (CanJsonLog (..))
import           Pos.Util.UserSecret            (HasUserSecret (..))
import           Pos.Util.Util                  (postfixLFields)
import           Pos.Wallet.Redirect            (MonadBlockchainInfo (..),
                                                 MonadUpdates (..),
                                                 applyLastUpdateWebWallet,
                                                 blockchainSlotDurationWebWallet,
                                                 connectedPeersWebWallet,
                                                 localChainDifficultyWebWallet,
                                                 networkChainDifficultyWebWallet,
                                                 waitForUpdateWebWallet)
import           Pos.Wallet.SscType             (WalletSscType)
import           Pos.Wallet.Web.Account         (AccountMode)
import           Pos.Wallet.Web.ClientTypes     (AccountId)
import           Pos.Wallet.Web.Sockets.ConnSet (ConnectionsVar)
import           Pos.Wallet.Web.State.State     (WalletState)
import           Pos.Wallet.Web.Tracking        (MonadBListener (..), onApplyTracking,
                                                 onRollbackTracking)
import           Pos.WorkMode                   (MinWorkMode, RealModeContext (..),
                                                 TxpExtra_TMP)

data WalletWebModeContext = WalletWebModeContext
    { wwmcWalletState     :: !WalletState
    , wwmcConnectionsVar  :: !ConnectionsVar
    , wwmcRealModeContext :: !(RealModeContext WalletSscType)
    }

makeLensesWith postfixLFields ''WalletWebModeContext

instance HasSscContext WalletSscType WalletWebModeContext where
    sscContext = wwmcRealModeContext_L . sscContext

instance HasPrimaryKey WalletWebModeContext where
    primaryKey = wwmcRealModeContext_L . primaryKey

instance HasReportingContext WalletWebModeContext  where
    reportingContext = wwmcRealModeContext_L . reportingContext

instance HasUserSecret WalletWebModeContext where
    userSecret = wwmcRealModeContext_L . userSecret

instance HasShutdownContext WalletWebModeContext where
    shutdownContext = wwmcRealModeContext_L . shutdownContext

instance HasNodeContext WalletSscType WalletWebModeContext where
    nodeContext = wwmcRealModeContext_L . nodeContext

instance HasSlottingVar WalletWebModeContext where
    slottingTimestamp = wwmcRealModeContext_L . slottingTimestamp
    slottingVar = wwmcRealModeContext_L . slottingVar

instance HasLens WalletState WalletWebModeContext WalletState where
    lensOf = wwmcWalletState_L

instance HasLens ConnectionsVar WalletWebModeContext ConnectionsVar where
    lensOf = wwmcConnectionsVar_L

instance {-# OVERLAPPABLE #-}
    HasLens tag (RealModeContext WalletSscType) r =>
    HasLens tag WalletWebModeContext r
  where
    lensOf = wwmcRealModeContext_L . lensOf @tag

instance HasLoggerName' WalletWebModeContext where
    loggerName = wwmcRealModeContext_L . loggerName

instance HasSlogContext WalletWebModeContext where
    slogContext = wwmcRealModeContext_L . slogContext

instance HasSlogGState WalletWebModeContext where
    slogGState = wwmcRealModeContext_L . slogGState

instance HasJsonLogConfig WalletWebModeContext where
    jsonLogConfig = wwmcRealModeContext_L . jsonLogConfig

data WalletWebModeContextTag

instance HasLens WalletWebModeContextTag WalletWebModeContext WalletWebModeContext where
    lensOf = identity

type WalletWebMode = Mtl.ReaderT WalletWebModeContext Production

type MonadWalletWebMode' ssc ctx m =
    ( MinWorkMode m
    , MonadBaseControl IO m
    , MonadMask m
    , MonadSlots ctx m
    , MonadGState m
    , MonadBlockDB ssc m
    , MonadTxpMem TxpExtra_TMP ctx m
    , SscHelpersClass ssc
    , MonadRecoveryInfo m
    , MonadBListener m
    , MonadReader ctx m
    , MonadKnownPeers m
    , MonadFormatPeers m
    , HasLens StateLock ctx StateLock
    , HasLens GenesisUtxo ctx GenesisUtxo
    , HasReportingContext ctx
    , HasShutdownContext ctx
    , HasCoreConstants
    , AccountMode ctx m
    , MonadBlockchainInfo m
    , MonadBalances m
    , MonadUpdates m
    , MonadTxHistory ssc m
    , MonadAddresses m
    , AddrData m ~ (AccountId, PassPhrase)
    )

type MonadWalletWebMode ctx m = MonadWalletWebMode' WalletSscType ctx m

type MonadWebSockets ctx =
    ( HasLens' ctx ConnectionsVar
    )

type MonadFullWalletWebMode ctx m =
    ( MonadWalletWebMode ctx m
    , MonadWebSockets ctx
    )

instance (HasCoreConstants, MonadSlotsData ctx WalletWebMode)
      => MonadSlots ctx WalletWebMode
  where
    getCurrentSlot = getCurrentSlotSum
    getCurrentSlotBlocking = getCurrentSlotBlockingSum
    getCurrentSlotInaccurate = getCurrentSlotInaccurateSum
    currentTimeSlotting = currentTimeSlottingSum

instance {-# OVERLAPPING #-} HasLoggerName WalletWebMode where
    getLoggerName = getLoggerNameDefault
    modifyLoggerName = modifyLoggerNameDefault

instance {-# OVERLAPPING #-} CanJsonLog WalletWebMode where
    jsonLog = jsonLogDefault

instance MonadDBRead WalletWebMode where
    dbGet = dbGetDefault
    dbIterSource = dbIterSourceDefault

instance MonadDB WalletWebMode where
    dbPut = dbPutDefault
    dbWriteBatch = dbWriteBatchDefault
    dbDelete = dbDeleteDefault

instance HasCoreConstants =>
         MonadBlockDBGenericWrite (BlockHeader WalletSscType) (Block WalletSscType) Undo WalletWebMode where
    dbPutBlund = dbPutBlundDefault

instance HasCoreConstants =>
         MonadBlockDBGeneric (BlockHeader WalletSscType) (Block WalletSscType) Undo WalletWebMode
  where
    dbGetBlock  = dbGetBlockDefault @WalletSscType
    dbGetUndo   = dbGetUndoDefault @WalletSscType
    dbGetHeader = dbGetHeaderDefault @WalletSscType

instance HasCoreConstants =>
         MonadBlockDBGeneric (Some IsHeader) (SscBlock WalletSscType) () WalletWebMode
  where
    dbGetBlock  = dbGetBlockSscDefault @WalletSscType
    dbGetUndo   = dbGetUndoSscDefault @WalletSscType
    dbGetHeader = dbGetHeaderSscDefault @WalletSscType

instance MonadGState WalletWebMode where
    gsAdoptedBVData = gsAdoptedBVDataDefault

instance HasCoreConstants => MonadBListener WalletWebMode where
    onApplyBlocks = onApplyTracking
    onRollbackBlocks = onRollbackTracking

instance MonadUpdates WalletWebMode where
    waitForUpdate = waitForUpdateWebWallet
    applyLastUpdate = applyLastUpdateWebWallet

instance HasCoreConstants => MonadBlockchainInfo WalletWebMode where
    networkChainDifficulty = networkChainDifficultyWebWallet
    localChainDifficulty = localChainDifficultyWebWallet
    connectedPeers = connectedPeersWebWallet
    blockchainSlotDuration = blockchainSlotDurationWebWallet

instance HasCoreConstants => MonadBalances WalletWebMode where
    getOwnUtxos = getOwnUtxosDefault
    getBalance = getBalanceDefault

instance HasCoreConstants => MonadTxHistory WalletSscType WalletWebMode where
    getBlockHistory = getBlockHistoryDefault @WalletSscType
    getLocalHistory = getLocalHistoryDefault
    saveTx = saveTxDefault

instance MonadKnownPeers WalletWebMode where
    updatePeersBucket = OQ.Reader.updatePeersBucketReader (rmcOutboundQ . wwmcRealModeContext)

instance MonadFormatPeers WalletWebMode where
    formatKnownPeers = OQ.Reader.formatKnownPeersReader (rmcOutboundQ . wwmcRealModeContext)
