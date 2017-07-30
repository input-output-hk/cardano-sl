{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Pos.Wallet.Web.Mode
       ( WalletWebMode
       , WalletWebModeContextTag
       , WalletWebModeContext(..)
       ) where

import           Universum

import           Control.Lens                  (makeLensesWith)
import qualified Control.Monad.Reader          as Mtl
import           Ether.Internal                (HasLens (..))
import           Mockable                      (Production)
import           System.Wlog                   (HasLoggerName (..))

import           Pos.Block.Core                (Block, BlockHeader)
import           Pos.Block.Slog.Types          (HasSlogContext (..))
import           Pos.Block.Types               (Undo)
import           Pos.Context                   (HasNodeContext (..))
import           Pos.Core                      (HasPrimaryKey (..), IsHeader)
import           Pos.DB                        (MonadGState (..))
import           Pos.DB.Block                  (dbGetBlockDefault, dbGetBlockSscDefault,
                                                dbGetHeaderDefault, dbGetHeaderSscDefault,
                                                dbGetUndoDefault, dbGetUndoSscDefault,
                                                dbPutBlundDefault)
import           Pos.DB.Class                  (MonadBlockDBGeneric (..),
                                                MonadBlockDBGenericWrite (..),
                                                MonadDB (..), MonadDBRead (..))
import           Pos.DB.DB                     (gsAdoptedBVDataDefault)
import           Pos.DB.Rocks                  (dbDeleteDefault, dbGetDefault,
                                                dbIterSourceDefault, dbPutDefault,
                                                dbWriteBatchDefault)

import           Pos.Client.Txp.Balances       (MonadBalances (..), getBalanceDefault,
                                                getOwnUtxosDefault)
import           Pos.Client.Txp.History        (MonadTxHistory (..),
                                                getBlockHistoryDefault,
                                                getLocalHistoryDefault, saveTxDefault)
import           Pos.Discovery                 (HasDiscoveryContextSum (..),
                                                MonadDiscovery (..), findPeersSum,
                                                getPeersSum)
import           Pos.Reporting                 (HasReportingContext (..))
import           Pos.Shutdown                  (HasShutdownContext (..))
import           Pos.Slotting.Class            (MonadSlots (..))
import           Pos.Slotting.Impl.Sum         (currentTimeSlottingSum,
                                                getCurrentSlotBlockingSum,
                                                getCurrentSlotInaccurateSum,
                                                getCurrentSlotSum)
import           Pos.Slotting.MemState         (HasSlottingVar (..), MonadSlotsData (..),
                                                getAllEpochIndicesDefault,
                                                getCurrentEpochIndexDefault,
                                                getCurrentEpochSlottingDataDefault,
                                                getEpochSlottingDataDefault,
                                                getNextEpochIndexDefault,
                                                getNextEpochSlottingDataDefault,
                                                getSystemStartDefault,
                                                putEpochSlottingDataDefault,
                                                waitCurrentEpochEqualsDefault)
import           Pos.Ssc.Class.Types           (HasSscContext (..), SscBlock)
import           Pos.Util                      (Some (..))
import           Pos.Util.JsonLog              (HasJsonLogConfig (..), jsonLogDefault)
import           Pos.Util.LoggerName           (HasLoggerName' (..), getLoggerNameDefault,
                                                modifyLoggerNameDefault)
import           Pos.Util.TimeWarp             (CanJsonLog (..))
import           Pos.Util.UserSecret           (HasUserSecret (..))
import           Pos.Util.Util                 (postfixLFields)
import           Pos.Wallet.Redirect           (MonadBlockchainInfo (..),
                                                MonadUpdates (..),
                                                applyLastUpdateWebWallet,
                                                blockchainSlotDurationWebWallet,
                                                connectedPeersWebWallet,
                                                localChainDifficultyWebWallet,
                                                networkChainDifficultyWebWallet,
                                                waitForUpdateWebWallet)
import           Pos.Wallet.SscType            (WalletSscType)
import           Pos.Wallet.Web.BListener      (MonadBListener (..), onApplyTracking,
                                                onRollbackTracking)
import           Pos.Wallet.Web.Server.Sockets (ConnectionsVar)
import           Pos.Wallet.Web.State.State    (WalletState)
import           Pos.Wallet.Web.Tracking       (MonadWalletTracking (..),
                                                syncWalletOnImportWebWallet,
                                                txMempoolToModifierWebWallet)
import           Pos.WorkMode                  (RealModeContext)



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

instance HasDiscoveryContextSum WalletWebModeContext where
    discoveryContextSum = wwmcRealModeContext_L . discoveryContextSum

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
    slogContextL = wwmcRealModeContext_L . slogContextL

instance HasJsonLogConfig WalletWebModeContext where
    jsonLogConfig = wwmcRealModeContext_L . jsonLogConfig

data WalletWebModeContextTag

instance HasLens WalletWebModeContextTag WalletWebModeContext WalletWebModeContext where
    lensOf = identity

type WalletWebMode = Mtl.ReaderT WalletWebModeContext Production

instance MonadSlotsData WalletWebMode where
    getSystemStartM = getSystemStartDefault
    getAllEpochIndicesM = getAllEpochIndicesDefault
    getCurrentEpochIndexM = getCurrentEpochIndexDefault
    getCurrentEpochSlottingDataM = getCurrentEpochSlottingDataDefault
    getNextEpochIndexM = getNextEpochIndexDefault
    getNextEpochSlottingDataM = getNextEpochSlottingDataDefault
    getEpochSlottingDataM = getEpochSlottingDataDefault
    putEpochSlottingDataM = putEpochSlottingDataDefault
    waitCurrentEpochEqualsM = waitCurrentEpochEqualsDefault

instance MonadSlots WalletWebMode where
    getCurrentSlot = getCurrentSlotSum
    getCurrentSlotBlocking = getCurrentSlotBlockingSum
    getCurrentSlotInaccurate = getCurrentSlotInaccurateSum
    currentTimeSlotting = currentTimeSlottingSum

instance MonadDiscovery WalletWebMode where
    getPeers = getPeersSum
    findPeers = findPeersSum

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

instance MonadBlockDBGenericWrite (BlockHeader WalletSscType) (Block WalletSscType) Undo WalletWebMode where
    dbPutBlund = dbPutBlundDefault

instance MonadBlockDBGeneric (BlockHeader WalletSscType) (Block WalletSscType) Undo WalletWebMode
  where
    dbGetBlock  = dbGetBlockDefault @WalletSscType
    dbGetUndo   = dbGetUndoDefault @WalletSscType
    dbGetHeader = dbGetHeaderDefault @WalletSscType

instance MonadBlockDBGeneric (Some IsHeader) (SscBlock WalletSscType) () WalletWebMode
  where
    dbGetBlock  = dbGetBlockSscDefault @WalletSscType
    dbGetUndo   = dbGetUndoSscDefault @WalletSscType
    dbGetHeader = dbGetHeaderSscDefault @WalletSscType

instance MonadGState WalletWebMode where
    gsAdoptedBVData = gsAdoptedBVDataDefault

instance MonadBListener WalletWebMode where
    onApplyBlocks = onApplyTracking
    onRollbackBlocks = onRollbackTracking

instance MonadUpdates WalletWebMode where
    waitForUpdate = waitForUpdateWebWallet
    applyLastUpdate = applyLastUpdateWebWallet

instance MonadBlockchainInfo WalletWebMode where
    networkChainDifficulty = networkChainDifficultyWebWallet
    localChainDifficulty = localChainDifficultyWebWallet
    connectedPeers = connectedPeersWebWallet
    blockchainSlotDuration = blockchainSlotDurationWebWallet

instance MonadBalances WalletWebMode where
    getOwnUtxos = getOwnUtxosDefault
    getBalance = getBalanceDefault

instance MonadTxHistory WalletSscType WalletWebMode where
    getBlockHistory = getBlockHistoryDefault @WalletSscType
    getLocalHistory = getLocalHistoryDefault
    saveTx = saveTxDefault

instance MonadWalletTracking WalletWebMode where
    syncWalletOnImport = syncWalletOnImportWebWallet . one
    txMempoolToModifier = txMempoolToModifierWebWallet
