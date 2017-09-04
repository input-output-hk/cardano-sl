{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Pos.Wallet.Web.Mode
       ( WalletWebMode
       , WalletWebModeContextTag
       , WalletWebModeContext(..)
       , MonadWalletWebMode
       , EmptyMempoolExt
       ) where

import           Universum

import           Control.Lens                   (makeLensesWith)
import qualified Control.Monad.Reader           as Mtl
import qualified Data.HashSet                   as HS
import           Data.List                      (partition)
import qualified Data.Map                       as M
import           Ether.Internal                 (HasLens (..))
import           Mockable                       (Production)
import           System.Wlog                    (HasLoggerName (..))

import           Pos.Block.Core                 (Block, BlockHeader)
import           Pos.Block.Slog                 (HasSlogContext (..), HasSlogGState (..))
import           Pos.Block.Types                (Undo)
import           Pos.Context                    (HasNodeContext (..))
import           Pos.Core                       (HasConfiguration, HasPrimaryKey (..),
                                                 IsHeader, isRedeemAddress)
import           Pos.Crypto                     (PassPhrase)
import           Pos.DB                         (MonadGState (..))
import           Pos.DB.Block                   (dbGetBlockDefault, dbGetBlockSscDefault,
                                                 dbGetHeaderDefault,
                                                 dbGetHeaderSscDefault, dbGetUndoDefault,
                                                 dbGetUndoSscDefault, dbPutBlundDefault)
import           Pos.DB.Class                   (MonadBlockDBGeneric (..),
                                                 MonadBlockDBGenericWrite (..),
                                                 MonadDB (..), MonadDBRead (..))
import           Pos.DB.DB                      (gsAdoptedBVDataDefault)
import           Pos.DB.Rocks                   (dbDeleteDefault, dbGetDefault,
                                                 dbIterSourceDefault, dbPutDefault,
                                                 dbWriteBatchDefault)
import           Pos.Recovery                   ()
import qualified Pos.Util.Modifier              as MM

import           Pos.Client.Txp.Addresses       (MonadAddresses (..))
import           Pos.Client.Txp.Balances        (MonadBalances (..), getBalanceDefault)
import           Pos.Client.Txp.History         (MonadTxHistory (..),
                                                 getBlockHistoryDefault,
                                                 getLocalHistoryDefault, saveTxDefault)
import           Pos.Infra.Configuration        (HasInfraConfiguration)
import           Pos.KnownPeers                 (MonadFormatPeers (..),
                                                 MonadKnownPeers (..))
import           Pos.Network.Types              (HasNodeType (..))
import           Pos.Reporting                  (HasReportingContext (..))
import           Pos.Shutdown                   (HasShutdownContext (..))
import           Pos.Slotting.Class             (MonadSlots (..))
import           Pos.Slotting.Impl.Sum          (currentTimeSlottingSum,
                                                 getCurrentSlotBlockingSum,
                                                 getCurrentSlotInaccurateSum,
                                                 getCurrentSlotSum)
import           Pos.Slotting.MemState          (HasSlottingVar (..), MonadSlotsData)
import           Pos.Ssc.Class.Types            (HasSscContext (..), SscBlock)
import           Pos.Ssc.GodTossing             (HasGtConfiguration)
import           Pos.Txp                        (MempoolExt, MonadTxpLocal (..),
                                                 addrBelongsToSet, getUtxoModifier,
                                                 txNormalize, txProcessTransaction)
import qualified Pos.Txp.DB                     as DB
import           Pos.Util                       (Some (..))
import           Pos.Util.CompileInfo           (HasCompileInfo)
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
import           Pos.Wallet.Web.Account         (AccountMode)
import           Pos.Wallet.Web.ClientTypes     (AccountId)
import           Pos.Wallet.Web.Sockets.ConnSet (ConnectionsVar)
import           Pos.Wallet.Web.State.State     (WalletState, getWalletUtxo)
import           Pos.Wallet.Web.Tracking        (MonadBListener (..), onApplyTracking,
                                                 onRollbackTracking)
import           Pos.WorkMode                   (EmptyMempoolExt, RealModeContext (..),
                                                 WorkMode)

data WalletWebModeContext = WalletWebModeContext
    { wwmcWalletState     :: !WalletState
    , wwmcConnectionsVar  :: !ConnectionsVar
    , wwmcRealModeContext :: !(RealModeContext EmptyMempoolExt)
    }

makeLensesWith postfixLFields ''WalletWebModeContext

instance HasSscContext WalletWebModeContext where
    sscContext = wwmcRealModeContext_L . sscContext

instance HasPrimaryKey WalletWebModeContext where
    primaryKey = wwmcRealModeContext_L . primaryKey

instance HasReportingContext WalletWebModeContext  where
    reportingContext = wwmcRealModeContext_L . reportingContext

instance HasUserSecret WalletWebModeContext where
    userSecret = wwmcRealModeContext_L . userSecret

instance HasShutdownContext WalletWebModeContext where
    shutdownContext = wwmcRealModeContext_L . shutdownContext

instance HasNodeContext WalletWebModeContext where
    nodeContext = wwmcRealModeContext_L . nodeContext

instance HasSlottingVar WalletWebModeContext where
    slottingTimestamp = wwmcRealModeContext_L . slottingTimestamp
    slottingVar = wwmcRealModeContext_L . slottingVar

instance HasLens WalletState WalletWebModeContext WalletState where
    lensOf = wwmcWalletState_L

instance HasLens ConnectionsVar WalletWebModeContext ConnectionsVar where
    lensOf = wwmcConnectionsVar_L

instance {-# OVERLAPPABLE #-}
    HasLens tag (RealModeContext EmptyMempoolExt) r =>
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

instance HasNodeType WalletWebModeContext where
    getNodeType = getNodeType . wwmcRealModeContext

data WalletWebModeContextTag

instance HasLens WalletWebModeContextTag WalletWebModeContext WalletWebModeContext where
    lensOf = identity

type WalletWebMode = Mtl.ReaderT WalletWebModeContext Production

-- This constraint used to be abstract (a list of classes), but specifying a
-- concrete monad is quite likely more performant.
type MonadWalletWebMode ctx m =
    ( WorkMode ctx m
    , AccountMode ctx m
    , MonadBlockchainInfo m
    , MonadBalances m
    , MonadUpdates m
    , MonadTxHistory m
    , MonadAddresses m
    , AddrData m ~ (AccountId, PassPhrase)
    , HasLens ConnectionsVar ctx ConnectionsVar
    )

instance (HasConfiguration, HasInfraConfiguration, MonadSlotsData ctx WalletWebMode)
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

instance HasConfiguration => MonadDBRead WalletWebMode where
    dbGet = dbGetDefault
    dbIterSource = dbIterSourceDefault

instance HasConfiguration => MonadDB WalletWebMode where
    dbPut = dbPutDefault
    dbWriteBatch = dbWriteBatchDefault
    dbDelete = dbDeleteDefault

instance (HasConfiguration, HasGtConfiguration) =>
         MonadBlockDBGenericWrite BlockHeader Block Undo WalletWebMode where
    dbPutBlund = dbPutBlundDefault

instance (HasConfiguration, HasGtConfiguration) =>
         MonadBlockDBGeneric BlockHeader Block Undo WalletWebMode
  where
    dbGetBlock  = dbGetBlockDefault
    dbGetUndo   = dbGetUndoDefault
    dbGetHeader = dbGetHeaderDefault

instance (HasConfiguration, HasGtConfiguration) =>
         MonadBlockDBGeneric (Some IsHeader) SscBlock () WalletWebMode
  where
    dbGetBlock  = dbGetBlockSscDefault
    dbGetUndo   = dbGetUndoSscDefault
    dbGetHeader = dbGetHeaderSscDefault

instance HasConfiguration => MonadGState WalletWebMode where
    gsAdoptedBVData = gsAdoptedBVDataDefault

instance (HasConfiguration, HasInfraConfiguration, HasCompileInfo) =>
         MonadBListener WalletWebMode where
    onApplyBlocks = onApplyTracking
    onRollbackBlocks = onRollbackTracking

instance MonadUpdates WalletWebMode where
    waitForUpdate = waitForUpdateWebWallet
    applyLastUpdate = applyLastUpdateWebWallet

instance (HasConfiguration, HasGtConfiguration, HasInfraConfiguration) =>
         MonadBlockchainInfo WalletWebMode where
    networkChainDifficulty = networkChainDifficultyWebWallet
    localChainDifficulty = localChainDifficultyWebWallet
    connectedPeers = connectedPeersWebWallet
    blockchainSlotDuration = blockchainSlotDurationWebWallet

instance HasConfiguration => MonadBalances WalletWebMode where
    getOwnUtxos addrs = do
        let (redeemAddrs, commonAddrs) = partition isRedeemAddress addrs

        updates <- getUtxoModifier
        commonUtxo <- if null commonAddrs then pure mempty
                        else getWalletUtxo
        redeemUtxo <- if null redeemAddrs then pure mempty
                        else DB.getFilteredUtxo redeemAddrs

        let allUtxo = MM.modifyMap updates $ commonUtxo <> redeemUtxo
            addrsSet = HS.fromList addrs
        pure $ M.filter (`addrBelongsToSet` addrsSet) allUtxo

    getBalance = getBalanceDefault

instance ( HasConfiguration
         , HasGtConfiguration
         , HasInfraConfiguration
         , HasCompileInfo
         ) =>
         MonadTxHistory WalletWebMode where
    getBlockHistory = getBlockHistoryDefault
    getLocalHistory = getLocalHistoryDefault
    saveTx = saveTxDefault

instance MonadKnownPeers WalletWebMode where
    updatePeersBucket = OQ.Reader.updatePeersBucketReader (rmcOutboundQ . wwmcRealModeContext)

instance MonadFormatPeers WalletWebMode where
    formatKnownPeers = OQ.Reader.formatKnownPeersReader (rmcOutboundQ . wwmcRealModeContext)

type instance MempoolExt WalletWebMode = EmptyMempoolExt

instance (HasConfiguration, HasInfraConfiguration, HasCompileInfo) =>
         MonadTxpLocal WalletWebMode where
    txpNormalize = txNormalize
    txpProcessTx = txProcessTransaction
