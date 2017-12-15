{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Pos.Wallet.Web.Mode
       ( WalletWebMode
       , WalletWebModeContextTag
       , WalletWebModeContext(..)
       , MonadWalletWebMode
       , MonadWalletWebSockets
       , MonadFullWalletWebMode

       , getBalanceDefault
       , getOwnUtxosDefault
       , getNewAddressWebWallet
       ) where

import           Universum

import qualified Control.Concurrent.STM as STM
import           Control.Lens (makeLensesWith)
import           Control.Monad.Catch (MonadMask)
import qualified Control.Monad.Reader as Mtl
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Crypto.Random (MonadRandom)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import           Data.List (partition)
import qualified Data.Map.Strict as M
import           Ether.Internal (HasLens (..))
import           Mockable (Production)
import           System.Wlog (HasLoggerName (..))

import           Pos.Block.Slog (HasSlogContext (..), HasSlogGState (..))
import           Pos.Client.KeyStorage (MonadKeys (..), MonadKeysRead (..), getSecretDefault,
                                        modifySecretDefault)
import           Pos.Client.Txp.Addresses (MonadAddresses (..))
import           Pos.Client.Txp.Balances (MonadBalances (..))
import           Pos.Client.Txp.History (MonadTxHistory (..), getBlockHistoryDefault,
                                         getLocalHistoryDefault, saveTxDefault)
import           Pos.Client.Txp.Network (submitTxRaw)
import           Pos.Communication (SendActions (..))
import           Pos.Communication.Limits (HasAdoptedBlockVersionData (..))
import           Pos.Context (HasNodeContext (..))
import           Pos.Core (Address, Coin, HasConfiguration, HasPrimaryKey (..), isRedeemAddress,
                           largestHDAddressBoot, mkCoin)
import           Pos.Crypto (PassPhrase)
import           Pos.DB (MonadGState (..))
import           Pos.DB.Block (dbGetSerBlockRealDefault, dbGetSerUndoRealDefault,
                               dbPutSerBlundRealDefault)
import           Pos.DB.Class (MonadDB (..), MonadDBRead (..))
import           Pos.DB.DB (gsAdoptedBVDataDefault)
import           Pos.DB.Rocks (dbDeleteDefault, dbGetDefault, dbIterSourceDefault, dbPutDefault,
                               dbWriteBatchDefault)
import           Pos.Infra.Configuration (HasInfraConfiguration)
import           Pos.KnownPeers (MonadFormatPeers (..), MonadKnownPeers (..))
import           Pos.Launcher (HasConfigurations)
import           Pos.Network.Types (HasNodeType (..))
import           Pos.Recovery ()
import           Pos.Recovery.Info (MonadRecoveryInfo)
import           Pos.Reporting (HasReportingContext (..), MonadReporting)
import           Pos.Shutdown (HasShutdownContext (..))
import           Pos.Slotting.Class (MonadSlots (..))
import           Pos.Slotting.Impl.Sum (currentTimeSlottingSum, getCurrentSlotBlockingSum,
                                        getCurrentSlotInaccurateSum, getCurrentSlotSum)
import           Pos.Slotting.MemState (HasSlottingVar (..), MonadSlotsData)
import           Pos.Ssc (HasSscConfiguration)
import           Pos.Ssc.Types (HasSscContext (..))
import           Pos.StateLock (StateLock)
import           Pos.Txp (MempoolExt, MonadTxpLocal (..), MonadTxpMem, Utxo, addrBelongsToSet,
                          applyUtxoModToAddrCoinMap, getUtxoModifier)
import qualified Pos.Txp.DB as DB
import           Pos.Util (postfixLFields)
import           Pos.Util.CompileInfo (HasCompileInfo)
import           Pos.Util.JsonLog (HasJsonLogConfig (..), jsonLogDefault)
import           Pos.Util.LoggerName (HasLoggerName' (..), getLoggerNameDefault,
                                      modifyLoggerNameDefault)
import qualified Pos.Util.Modifier as MM
import qualified Pos.Util.OutboundQueue as OQ.Reader
import           Pos.Util.TimeWarp (CanJsonLog (..))
import           Pos.Util.UserSecret (HasUserSecret (..))
import           Pos.Wallet.Web.Networking (MonadWalletSendActions (..))
import           Pos.Wallet.Web.Util (decodeCTypeOrFail)
import           Pos.WorkMode (MinWorkMode, RealModeContext (..))

import           Pos.Wallet.Redirect (MonadBlockchainInfo (..), MonadUpdates (..),
                                      applyLastUpdateWebWallet, blockchainSlotDurationWebWallet,
                                      connectedPeersWebWallet, localChainDifficultyWebWallet,
                                      networkChainDifficultyWebWallet, txpNormalizeWebWallet,
                                      txpProcessTxWebWallet, waitForUpdateWebWallet)
import           Pos.Wallet.WalletMode (WalletMempoolExt)
import           Pos.Wallet.Web.Account (AccountMode, GenSeed (RandomSeed))
import           Pos.Wallet.Web.ClientTypes (AccountId, cadId)
import           Pos.Wallet.Web.Methods (MonadWalletLogic, newAddress)
import           Pos.Wallet.Web.Sockets.Connection (MonadWalletWebSockets)
import           Pos.Wallet.Web.Sockets.ConnSet (ConnectionsVar)
import           Pos.Wallet.Web.State (MonadWalletDB, MonadWalletDBRead, WalletState,
                                       getWalletBalancesAndUtxo, getWalletUtxo)
import           Pos.Wallet.Web.Tracking (MonadBListener (..), onApplyBlocksWebWallet,
                                          onRollbackBlocksWebWallet)

data WalletWebModeContext = WalletWebModeContext
    { wwmcWalletState     :: !WalletState
    , wwmcConnectionsVar  :: !ConnectionsVar
    , wwmcSendActions     :: !(STM.TMVar (SendActions WalletWebMode))
    , wwmcRealModeContext :: !(RealModeContext WalletMempoolExt)
    }

-- It's here because of TH for lens
type WalletWebMode = Mtl.ReaderT WalletWebModeContext Production

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

instance HasConfiguration => MonadWalletDB WalletWebModeContext WalletWebMode

instance {-# OVERLAPPABLE #-}
    HasLens tag (RealModeContext WalletMempoolExt) r =>
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

----------------------------------------------------------------------------
-- Wallet modes
----------------------------------------------------------------------------

type MonadWalletWebMode ctx m =
    ( MinWorkMode m
    , MonadBaseControl IO m
    , MonadMask m
    , MonadSlots ctx m
    , MonadGState m
    , MonadDBRead m
    , MonadTxpMem WalletMempoolExt ctx m
    , MonadRecoveryInfo m
    , MonadBListener m
    , MonadReader ctx m
    , MonadKnownPeers m
    , MonadFormatPeers m
    , HasLens StateLock ctx StateLock
    , HasNodeType ctx
    , HasReportingContext ctx
    , HasShutdownContext ctx
    , AccountMode ctx m
    , MonadBlockchainInfo m
    , MonadBalances m
    , MonadUpdates m
    , MonadTxHistory m
    , MonadWalletDB ctx m
    , MonadKeys m
    , MonadAddresses m
    , MonadRandom m
    , AddrData m ~ (AccountId, PassPhrase)
    )

type MonadFullWalletWebMode ctx m =
    ( MonadWalletWebMode ctx m
    , MonadWalletWebSockets ctx m
    , MonadWalletSendActions m
    , MonadReporting ctx m
    )

----------------------------------------------------------------------------
-- Instances for WalletWebMode
----------------------------------------------------------------------------

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
    dbGetSerBlock = dbGetSerBlockRealDefault
    dbGetSerUndo = dbGetSerUndoRealDefault

instance HasConfiguration => MonadDB WalletWebMode where
    dbPut = dbPutDefault
    dbWriteBatch = dbWriteBatchDefault
    dbDelete = dbDeleteDefault
    dbPutSerBlund = dbPutSerBlundRealDefault

instance HasConfiguration => MonadGState WalletWebMode where
    gsAdoptedBVData = gsAdoptedBVDataDefault

instance HasConfiguration => HasAdoptedBlockVersionData WalletWebMode where
    adoptedBVData = gsAdoptedBVData

instance (HasConfiguration, HasInfraConfiguration, HasCompileInfo)
       => MonadBListener WalletWebMode where
    onApplyBlocks = onApplyBlocksWebWallet
    onRollbackBlocks = onRollbackBlocksWebWallet

instance MonadUpdates WalletWebMode where
    waitForUpdate = waitForUpdateWebWallet
    applyLastUpdate = applyLastUpdateWebWallet

instance (HasConfiguration, HasSscConfiguration, HasInfraConfiguration) =>
         MonadBlockchainInfo WalletWebMode where
    networkChainDifficulty = networkChainDifficultyWebWallet
    localChainDifficulty = localChainDifficultyWebWallet
    connectedPeers = connectedPeersWebWallet
    blockchainSlotDuration = blockchainSlotDurationWebWallet

type BalancesEnv ext ctx m =
    ( MonadDBRead m
    , MonadGState m
    , MonadWalletDBRead ctx m
    , MonadMask m
    , MonadTxpMem ext ctx m)

getOwnUtxosDefault :: BalancesEnv ext ctx m => [Address] -> m Utxo
getOwnUtxosDefault addrs = do
    let (redeemAddrs, commonAddrs) = partition isRedeemAddress addrs

    updates <- getUtxoModifier
    commonUtxo <- if null commonAddrs then pure mempty
                  else getWalletUtxo
    redeemUtxo <- if null redeemAddrs then pure mempty
                  else DB.getFilteredUtxo redeemAddrs

    let allUtxo = MM.modifyMap updates $ commonUtxo <> redeemUtxo
        addrsSet = HS.fromList addrs
    pure $ M.filter (`addrBelongsToSet` addrsSet) allUtxo

-- | `BalanceDB` isn't used here anymore, because
-- 1) It doesn't represent actual balances of addresses, but it represents _stakes_
-- 2) Local utxo is now cached, and deriving balances from it is not
--    so bad for performance now
getBalanceDefault :: BalancesEnv ext ctx m => Address -> m Coin
getBalanceDefault addr = do
    balancesAndUtxo <- getWalletBalancesAndUtxo
    fromMaybe (mkCoin 0) .
        HM.lookup addr .
        flip applyUtxoModToAddrCoinMap balancesAndUtxo <$> getUtxoModifier

instance HasConfiguration => MonadBalances WalletWebMode where
    getOwnUtxos = getOwnUtxosDefault
    getBalance = getBalanceDefault

instance (HasConfiguration, HasSscConfiguration, HasInfraConfiguration, HasCompileInfo)
        => MonadTxHistory WalletWebMode where
    getBlockHistory = getBlockHistoryDefault
    getLocalHistory = getLocalHistoryDefault
    saveTx = saveTxDefault

instance MonadKnownPeers WalletWebMode where
    updatePeersBucket = OQ.Reader.updatePeersBucketReader (rmcOutboundQ . wwmcRealModeContext)

instance MonadFormatPeers WalletWebMode where
    formatKnownPeers = OQ.Reader.formatKnownPeersReader (rmcOutboundQ . wwmcRealModeContext)

type instance MempoolExt WalletWebMode = WalletMempoolExt

instance (HasConfiguration, HasInfraConfiguration, HasCompileInfo) =>
         MonadTxpLocal WalletWebMode where
    txpNormalize = txpNormalizeWebWallet
    txpProcessTx = txpProcessTxWebWallet

instance MonadKeysRead WalletWebMode where
    getSecret = getSecretDefault

instance MonadKeys WalletWebMode where
    modifySecret = modifySecretDefault

instance HasConfigurations => MonadWalletSendActions WalletWebMode where
    sendTxToNetwork tx = do
        saVar <- view wwmcSendActions_L
        saMB <- atomically $ STM.tryReadTMVar saVar
        let sa = fromMaybe (error "Wallet's SendActions isn't initialized") saMB
        submitTxRaw (enqueueMsg sa) tx

getNewAddressWebWallet
    :: MonadWalletLogic ctx m
    => (AccountId, PassPhrase) -> m Address
getNewAddressWebWallet (accId, passphrase) = do
    clientAddress <- newAddress RandomSeed passphrase accId
    decodeCTypeOrFail (cadId clientAddress)

instance (HasConfigurations, HasCompileInfo)
      => MonadAddresses Pos.Wallet.Web.Mode.WalletWebMode where
    type AddrData Pos.Wallet.Web.Mode.WalletWebMode = (AccountId, PassPhrase)
    -- We rely on the fact that Daedalus always uses HD addresses with
    -- BootstrapEra distribution.
    getFakeChangeAddress = pure largestHDAddressBoot
    getNewAddress = getNewAddressWebWallet
