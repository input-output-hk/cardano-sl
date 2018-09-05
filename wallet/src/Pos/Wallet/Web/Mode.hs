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
       , realModeToWalletWebMode

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
import           UnliftIO (MonadUnliftIO)

import           Pos.Chain.Block (HasSlogContext (..), HasSlogGState (..))
import           Pos.Chain.Ssc (HasSscContext (..))
import           Pos.Chain.Txp (Utxo, addrBelongsToSet,
                     applyUtxoModToAddrCoinMap)
import           Pos.Client.KeyStorage (MonadKeys (..), MonadKeysRead (..),
                     getPublicDefault, getSecretDefault, modifyPublicDefault,
                     modifySecretDefault)
import           Pos.Client.Txp.Addresses (MonadAddresses (..))
import           Pos.Client.Txp.Balances (MonadBalances (..))
import           Pos.Client.Txp.History (MonadTxHistory (..),
                     getBlockHistoryDefault, getLocalHistoryDefault,
                     saveTxDefault)
import           Pos.Context (HasNodeContext (..))
import           Pos.Core (Address, Coin, HasPrimaryKey (..), isRedeemAddress,
                     largestHDAddressBoot, mkCoin)
import           Pos.Core.JsonLog (CanJsonLog (..))
import           Pos.Core.Reporting (HasMisbehaviorMetrics (..),
                     MonadReporting (..), Reporter (..))
import           Pos.Core.Slotting (HasSlottingVar (..), MonadSlotsData)
import           Pos.Crypto (PassPhrase)
import           Pos.DB (MonadGState (..))
import           Pos.DB.Block (dbGetSerBlockRealDefault,
                     dbGetSerBlundRealDefault, dbGetSerUndoRealDefault,
                     dbPutSerBlundsRealDefault)
import           Pos.DB.Class (MonadDB (..), MonadDBRead (..))
import           Pos.DB.DB (gsAdoptedBVDataDefault)
import           Pos.DB.Rocks (dbDeleteDefault, dbGetDefault,
                     dbIterSourceDefault, dbPutDefault, dbWriteBatchDefault)
import           Pos.DB.Txp (MempoolExt, MonadTxpLocal (..), MonadTxpMem,
                     getUtxoModifier, withTxpLocalData)
import qualified Pos.DB.Txp as DB
import           Pos.Infra.Network.Types (HasNodeType (..))
import           Pos.Infra.Recovery.Info (MonadRecoveryInfo)
import           Pos.Infra.Shutdown (HasShutdownContext (..))
import           Pos.Infra.Slotting.Class (MonadSlots (..))
import           Pos.Infra.Slotting.Impl (currentTimeSlottingSimple,
                     getCurrentSlotBlockingSimple,
                     getCurrentSlotInaccurateSimple, getCurrentSlotSimple)
import           Pos.Infra.StateLock (StateLock)
import           Pos.Infra.Util.JsonLog.Events (HasJsonLogConfig (..),
                     jsonLogDefault)
import           Pos.Recovery ()
import           Pos.Util (postfixLFields)
import           Pos.Util.LoggerName (HasLoggerName' (..), askLoggerNameDefault,
                     modifyLoggerNameDefault)
import qualified Pos.Util.Modifier as MM
import           Pos.Util.UserPublic (HasUserPublic (..))
import           Pos.Util.UserSecret (HasUserSecret (..))
import           Pos.Util.Util (HasLens (..))
import           Pos.Util.Wlog (HasLoggerName (..))
import           Pos.Wallet.Web.Tracking.Types (SyncQueue)
import           Pos.WorkMode (MinWorkMode, RealMode, RealModeContext (..))

import           Pos.Wallet.Redirect (MonadBlockchainInfo (..),
                     MonadUpdates (..), applyLastUpdateWebWallet,
                     blockchainSlotDurationWebWallet, connectedPeersWebWallet,
                     localChainDifficultyWebWallet,
                     networkChainDifficultyWebWallet, txpNormalizeWebWallet,
                     txpProcessTxWebWallet, waitForUpdateWebWallet)
import           Pos.Wallet.WalletMode (WalletMempoolExt)
import           Pos.Wallet.Web.Account (AccountMode, GenSeed (RandomSeed))
import           Pos.Wallet.Web.ClientTypes (AccountId)
import           Pos.Wallet.Web.Methods.Logic (MonadWalletLogic, newAddress_)
import           Pos.Wallet.Web.Sockets.Connection (MonadWalletWebSockets)
import           Pos.Wallet.Web.Sockets.ConnSet (ConnectionsVar)
import           Pos.Wallet.Web.State (WalletDB, WalletDbReader,
                     askWalletSnapshot, getWalletBalancesAndUtxo,
                     getWalletUtxo, wamAddress)
import           Pos.Wallet.Web.Tracking (MonadBListener (..),
                     onApplyBlocksWebWallet, onRollbackBlocksWebWallet)

data WalletWebModeContext = WalletWebModeContext
    { wwmcWalletState        :: !WalletDB
    , wwmcConnectionsVar     :: !ConnectionsVar
    , wwmcWalletSyncRequests :: !SyncQueue
    , wwmcRealModeContext    :: !(RealModeContext WalletMempoolExt)
    }

-- It's here because of TH for lens
type WalletWebMode = Mtl.ReaderT WalletWebModeContext IO

walletWebModeToRealMode
    :: WalletDB
    -> ConnectionsVar
    -> SyncQueue
    -> WalletWebMode t
    -> RealMode WalletMempoolExt t
walletWebModeToRealMode ws cv syncRequests act = do
    rmc <- ask
    lift $ runReaderT act (WalletWebModeContext ws cv syncRequests rmc)

realModeToWalletWebMode
    :: RealMode WalletMempoolExt t
    -> WalletWebMode t
realModeToWalletWebMode rm = Mtl.ask >>= \ctx ->
    let rmc = wwmcRealModeContext ctx
     in lift (Mtl.runReaderT rm rmc)

makeLensesWith postfixLFields ''WalletWebModeContext

instance HasLens SyncQueue WalletWebModeContext SyncQueue where
    lensOf = wwmcWalletSyncRequests_L

instance HasSscContext WalletWebModeContext where
    sscContext = wwmcRealModeContext_L . sscContext

instance HasPrimaryKey WalletWebModeContext where
    primaryKey = wwmcRealModeContext_L . primaryKey

-- FIXME alter it so that we never send logs for info-level reports, as I
-- think that's how it was prior.
instance MonadReporting WalletWebMode where
    report rt = Mtl.ask >>= \ctx ->
        liftIO (runReporter (rmcReporter (wwmcRealModeContext ctx)) rt)

instance HasMisbehaviorMetrics WalletWebModeContext where
  misbehaviorMetrics = wwmcRealModeContext_L . misbehaviorMetrics

instance HasUserPublic WalletWebModeContext where
    userPublic = wwmcRealModeContext_L . userPublic

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
    , MonadRecoveryInfo ctx m
    , MonadBListener m
    , MonadReader ctx m
    , HasLens StateLock ctx StateLock
    , HasNodeType ctx
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
    , MonadReporting m
    , HasLens SyncQueue ctx SyncQueue
    )

----------------------------------------------------------------------------
-- Instances for WalletWebMode
----------------------------------------------------------------------------

instance MonadSlotsData ctx WalletWebMode => MonadSlots ctx WalletWebMode where
    getCurrentSlot = getCurrentSlotSimple
    getCurrentSlotBlocking = getCurrentSlotBlockingSimple
    getCurrentSlotInaccurate = getCurrentSlotInaccurateSimple
    currentTimeSlotting = currentTimeSlottingSimple

instance {-# OVERLAPPING #-} HasLoggerName WalletWebMode where
    askLoggerName = askLoggerNameDefault
    modifyLoggerName = modifyLoggerNameDefault

instance {-# OVERLAPPING #-} CanJsonLog WalletWebMode where
    jsonLog = jsonLogDefault

instance MonadDBRead WalletWebMode where
    dbGet = dbGetDefault
    dbIterSource = dbIterSourceDefault
    dbGetSerBlock = dbGetSerBlockRealDefault
    dbGetSerUndo = dbGetSerUndoRealDefault
    dbGetSerBlund = dbGetSerBlundRealDefault

instance MonadDB WalletWebMode where
    dbPut = dbPutDefault
    dbWriteBatch = dbWriteBatchDefault
    dbDelete = dbDeleteDefault
    dbPutSerBlunds = dbPutSerBlundsRealDefault

instance MonadGState WalletWebMode where
    gsAdoptedBVData = gsAdoptedBVDataDefault

instance MonadBListener WalletWebMode where
    onApplyBlocks = onApplyBlocksWebWallet
    onRollbackBlocks = onRollbackBlocksWebWallet

instance MonadUpdates WalletWebMode where
    waitForUpdate = waitForUpdateWebWallet
    applyLastUpdate = applyLastUpdateWebWallet

instance MonadBlockchainInfo WalletWebMode where
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

instance MonadBalances WalletWebMode where
    getOwnUtxos = const $ getOwnUtxosDefault
    getBalance = const $ getBalanceDefault

instance MonadTxHistory WalletWebMode where
    getBlockHistory = getBlockHistoryDefault
    getLocalHistory = getLocalHistoryDefault
    saveTx = saveTxDefault

type instance MempoolExt WalletWebMode = WalletMempoolExt

instance MonadTxpLocal WalletWebMode where
    txpNormalize = txpNormalizeWebWallet
    txpProcessTx = txpProcessTxWebWallet

instance MonadKeysRead WalletWebMode where
    getPublic = getPublicDefault
    getSecret = getSecretDefault

instance MonadKeys WalletWebMode where
    modifyPublic = modifyPublicDefault
    modifySecret = modifySecretDefault

getNewAddressWebWallet
    :: MonadWalletLogic ctx m
    => (AccountId, PassPhrase) -> m Address
getNewAddressWebWallet (accId, passphrase) = do
    ws <- askWalletSnapshot
    cAddrMeta <- newAddress_ ws RandomSeed passphrase accId
    return $ cAddrMeta ^. wamAddress

instance MonadAddresses Pos.Wallet.Web.Mode.WalletWebMode where
    type AddrData Pos.Wallet.Web.Mode.WalletWebMode = (AccountId, PassPhrase)
    -- We rely on the fact that Daedalus always uses HD addresses with
    -- BootstrapEra distribution.
    getFakeChangeAddress _ = pure largestHDAddressBoot
    getNewAddress _ = getNewAddressWebWallet
