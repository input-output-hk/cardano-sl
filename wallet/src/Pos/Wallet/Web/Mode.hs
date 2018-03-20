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

       , walletWebModeToRealMode

       , getBalanceDefault
       , getOwnUtxosDefault
       , getNewAddressWebWallet
       ) where

import           Universum

import           Control.Lens (makeLensesWith)
import qualified Control.Monad.Reader as Mtl
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Crypto.Random (MonadRandom)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import           Data.List (partition)
import qualified Data.Map.Strict as M
import           Mockable (LowLevelAsync, Mockable, Production)
import           System.Wlog (HasLoggerName (..))
import           UnliftIO (MonadUnliftIO)

import           Pos.Block.Slog (HasSlogContext (..), HasSlogGState (..))
import           Pos.Client.KeyStorage (MonadKeys (..), MonadKeysRead (..), getSecretDefault,
                                        modifySecretDefault)
import           Pos.Client.Txp.Addresses (MonadAddresses (..))
import           Pos.Client.Txp.Balances (MonadBalances (..))
import           Pos.Client.Txp.History (MonadTxHistory (..), getBlockHistoryDefault,
                                         getLocalHistoryDefault, saveTxDefault)
import           Pos.Context (HasNodeContext (..))
import           Pos.Core (Address, Coin, HasConfiguration, HasPrimaryKey (..), isRedeemAddress,
                           largestHDAddressBoot, mkCoin)
import           Pos.Crypto (PassPhrase)
import           Pos.DB (MonadGState (..))
import           Pos.DB.Block (dbGetSerBlockRealDefault, dbGetSerUndoRealDefault,
                               dbPutSerBlundsRealDefault)
import           Pos.DB.Class (MonadDB (..), MonadDBRead (..))
import           Pos.DB.DB (gsAdoptedBVDataDefault)
import           Pos.DB.Rocks (dbDeleteDefault, dbGetDefault, dbIterSourceDefault, dbPutDefault,
                               dbWriteBatchDefault)
import           Pos.Infra.Configuration (HasInfraConfiguration)
import           Pos.KnownPeers (MonadFormatPeers (..))
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
import           Pos.Txp (HasTxpConfiguration, MempoolExt, MonadTxpLocal (..), MonadTxpMem, Utxo,
                          addrBelongsToSet, applyUtxoModToAddrCoinMap, getUtxoModifier,
                          withTxpLocalData)
import qualified Pos.Txp.DB as DB
import           Pos.Util (postfixLFields)
import           Pos.Util.CompileInfo (HasCompileInfo)
import           Pos.Util.JsonLog (HasJsonLogConfig (..), jsonLogDefault)
import           Pos.Util.LoggerName (HasLoggerName' (..), askLoggerNameDefault,
                                      modifyLoggerNameDefault)
import qualified Pos.Util.Modifier as MM
import           Pos.Util.TimeWarp (CanJsonLog (..))
import           Pos.Util.UserSecret (HasUserSecret (..))
import           Pos.Util.Util (HasLens (..))
import           Pos.Wallet.Web.Util (decodeCTypeOrFail)
import           Pos.WorkMode (MinWorkMode, RealMode, RealModeContext (..))

import           Pos.Wallet.Redirect (MonadBlockchainInfo (..), MonadUpdates (..),
                                      applyLastUpdateWebWallet, blockchainSlotDurationWebWallet,
                                      connectedPeersWebWallet, localChainDifficultyWebWallet,
                                      networkChainDifficultyWebWallet, txpNormalizeWebWallet,
                                      txpProcessTxWebWallet, waitForUpdateWebWallet)
import           Pos.Wallet.WalletMode (WalletMempoolExt)
import           Pos.Wallet.Web.Account (AccountMode, GenSeed (RandomSeed))
import           Pos.Wallet.Web.ClientTypes (AccountId, cwamId)
import           Pos.Wallet.Web.Methods.Logic (MonadWalletLogic, newAddress_)
import           Pos.Wallet.Web.Methods.Misc (AddrCIdHashes, MonadConvertToAddr)
import           Pos.Wallet.Web.Sockets.Connection (MonadWalletWebSockets)
import           Pos.Wallet.Web.Sockets.ConnSet (ConnectionsVar)
import           Pos.Wallet.Web.State (WalletDB, WalletDbReader, getWalletBalancesAndUtxo,
                                       getWalletUtxo, askWalletSnapshot)
import           Pos.Wallet.Web.Tracking (MonadBListener (..), onApplyBlocksWebWallet,
                                          onRollbackBlocksWebWallet)

data WalletWebModeContext = WalletWebModeContext
    { wwmcWalletState     :: !WalletDB
    , wwmcConnectionsVar  :: !ConnectionsVar
    , wwmcHashes          :: !AddrCIdHashes
    , wwmcRealModeContext :: !(RealModeContext WalletMempoolExt)
    }

-- It's here because of TH for lens
type WalletWebMode = Mtl.ReaderT WalletWebModeContext Production

walletWebModeToRealMode
    :: WalletDB
    -> ConnectionsVar
    -> AddrCIdHashes
    -> WalletWebMode t
    -> RealMode WalletMempoolExt t
walletWebModeToRealMode ws cv cidHashes act = do
    rmc <- ask
    lift $ runReaderT act (WalletWebModeContext ws cv cidHashes rmc)

makeLensesWith postfixLFields ''WalletWebModeContext

instance HasLens AddrCIdHashes WalletWebModeContext AddrCIdHashes where
    lensOf = wwmcHashes_L

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

instance HasLens WalletDB WalletWebModeContext WalletDB where
    lensOf = wwmcWalletState_L

instance HasLens ConnectionsVar WalletWebModeContext ConnectionsVar where
    lensOf = wwmcConnectionsVar_L

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
    , MonadUnliftIO m
    , MonadBaseControl IO m
    , MonadMask m
    , MonadSlots ctx m
    , MonadGState m
    , MonadDBRead m
    , MonadTxpMem WalletMempoolExt ctx m
    , MonadRecoveryInfo m
    , MonadBListener m
    , MonadReader ctx m
    , MonadFormatPeers m
    , MonadConvertToAddr ctx m
    , HasLens StateLock ctx StateLock
    , HasNodeType ctx
    , HasReportingContext ctx
    , HasShutdownContext ctx
    , AccountMode ctx m
    , MonadBlockchainInfo m
    , MonadBalances m
    , MonadUpdates m
    , MonadTxHistory m
    , WalletDbReader ctx m
    , MonadKeys m
    , MonadAddresses m
    , MonadRandom m
    , AddrData m ~ (AccountId, PassPhrase)
    )

type MonadFullWalletWebMode ctx m =
    ( MonadWalletWebMode ctx m
    , MonadWalletWebSockets ctx m
    , MonadReporting ctx m
    , Mockable LowLevelAsync m
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
    askLoggerName = askLoggerNameDefault
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
    dbPutSerBlunds = dbPutSerBlundsRealDefault

instance HasConfiguration => MonadGState WalletWebMode where
    gsAdoptedBVData = gsAdoptedBVDataDefault

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
    , MonadUnliftIO m
    , MonadGState m
    , WalletDbReader ctx m
    , MonadMask m
    , MonadTxpMem ext ctx m
    )

getOwnUtxosDefault :: BalancesEnv ext ctx m => [Address] -> m Utxo
getOwnUtxosDefault addrs = do
    ws <- askWalletSnapshot
    let (redeemAddrs, commonAddrs) = partition isRedeemAddress addrs

    updates <- withTxpLocalData getUtxoModifier
    let commonUtxo = if null commonAddrs then mempty
                     else getWalletUtxo ws
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
    ws <- askWalletSnapshot
    updates <- withTxpLocalData getUtxoModifier
    let balancesAndUtxo = getWalletBalancesAndUtxo ws
    return $ fromMaybe (mkCoin 0) .
        HM.lookup addr $
        applyUtxoModToAddrCoinMap updates balancesAndUtxo

instance HasConfiguration => MonadBalances WalletWebMode where
    getOwnUtxos = getOwnUtxosDefault
    getBalance = getBalanceDefault

instance (HasConfiguration, HasSscConfiguration, HasTxpConfiguration, HasInfraConfiguration, HasCompileInfo)
        => MonadTxHistory WalletWebMode where
    getBlockHistory = getBlockHistoryDefault
    getLocalHistory = getLocalHistoryDefault
    saveTx = saveTxDefault

instance MonadFormatPeers WalletWebMode where
    -- Use the RealMode instance (ReaderT RealModeContext Production)
    formatKnownPeers formatter = Mtl.withReaderT wwmcRealModeContext (formatKnownPeers formatter)

type instance MempoolExt WalletWebMode = WalletMempoolExt

instance (HasConfiguration, HasInfraConfiguration, HasTxpConfiguration, HasCompileInfo) =>
         MonadTxpLocal WalletWebMode where
    txpNormalize = txpNormalizeWebWallet
    txpProcessTx = txpProcessTxWebWallet

instance MonadKeysRead WalletWebMode where
    getSecret = getSecretDefault

instance MonadKeys WalletWebMode where
    modifySecret = modifySecretDefault

getNewAddressWebWallet
    :: MonadWalletLogic ctx m
    => (AccountId, PassPhrase) -> m Address
getNewAddressWebWallet (accId, passphrase) = do
    ws <- askWalletSnapshot
    cAddrMeta <- newAddress_ ws RandomSeed passphrase accId
    decodeCTypeOrFail (cwamId cAddrMeta)

instance (HasConfigurations, HasCompileInfo)
      => MonadAddresses Pos.Wallet.Web.Mode.WalletWebMode where
    type AddrData Pos.Wallet.Web.Mode.WalletWebMode = (AccountId, PassPhrase)
    -- We rely on the fact that Daedalus always uses HD addresses with
    -- BootstrapEra distribution.
    getFakeChangeAddress = pure largestHDAddressBoot
    getNewAddress = getNewAddressWebWallet
