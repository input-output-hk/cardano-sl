{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS -fno-warn-unused-top-binds #-} -- for lenses

-- | Module which provides `MonadWalletWebMode` instance for tests

module Test.Pos.Wallet.Web.Mode
       ( WalletTestParams (..)
       , HasWalletTestParams (..)
       , WalletTestMode
       , WalletTestContext (..)
       , runWalletTestMode
       , WalletProperty
       , walletPropertyToProperty
       , walletPropertySpec

       , submitTxTestMode
       , getSentTxs
       ) where

import           Universum

import qualified Control.Concurrent.STM as STM
import           Control.Lens (lens, makeClassy, makeLensesWith)
import           Data.Default (def)
import           Formatting (bprint, build, formatToString, (%))
import qualified Formatting.Buildable
import qualified Prelude
import           Test.Hspec (Spec)
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck (Arbitrary (..), Property, Testable (..),
                     forAll, ioProperty)
import           Test.QuickCheck.Gen (Gen)
import           Test.QuickCheck.Monadic (PropertyM (..), monadic)

import           Pos.AllSecrets (HasAllSecrets (..))
import           Pos.Chain.Block (HasSlogGState (..), LastKnownHeader,
                     LastKnownHeaderTag)
import           Pos.Chain.Delegation (DelegationVar, HasDlgConfiguration)
import           Pos.Chain.Ssc (SscMemTag, SscState)
import           Pos.Chain.Txp (TxAux)
import           Pos.Client.KeyStorage (MonadKeys (..), MonadKeysRead (..),
                     getPublicDefault, getSecretDefault,
                     modifyPublicPureDefault, modifySecretPureDefault)
import           Pos.Client.Txp.Addresses (MonadAddresses (..))
import           Pos.Client.Txp.Balances (MonadBalances (..))
import           Pos.Client.Txp.History (MonadTxHistory (..),
                     getBlockHistoryDefault, getLocalHistoryDefault,
                     saveTxDefault)
import           Pos.Context (ConnectedPeers (..))
import           Pos.Core (Timestamp (..), largestHDAddressBoot)
import           Pos.Core.JsonLog (CanJsonLog (..))
import           Pos.Crypto (PassPhrase)
import           Pos.DB (MonadDB (..), MonadDBRead (..), MonadGState (..))
import qualified Pos.DB as DB
import           Pos.DB.Block (MonadBListener (..))
import qualified Pos.DB.Block as DB
import           Pos.DB.DB (gsAdoptedBVDataDefault)
import           Pos.DB.Lrc (LrcContext)
import           Pos.DB.Pure (DBPureVar)
import           Pos.DB.Txp (GenericTxpLocalData, MempoolExt,
                     MonadTxpLocal (..), TxpGlobalSettings, TxpHolderTag,
                     recordTxpMetrics, txNormalize, txProcessTransactionNoLock,
                     txpMemPool, txpTip)
import           Pos.DB.Update (UpdateContext)
import           Pos.Generator.Block (BlockGenMode)
import qualified Pos.GState as GS
import           Pos.Infra.Network.Types (HasNodeType (..), NodeType (..))
import           Pos.Infra.Recovery.Types (RecoveryHeader, RecoveryHeaderTag)
import           Pos.Infra.Reporting (MonadReporting (..))
import           Pos.Infra.Shutdown (HasShutdownContext (..),
                     ShutdownContext (..))
import           Pos.Infra.Slotting (HasSlottingVar (..), MonadSlots (..),
                     MonadSlotsData, SimpleSlottingStateVar,
                     mkSimpleSlottingStateVar)
import           Pos.Infra.StateLock (StateLock, StateLockMetrics (..),
                     newStateLock)
import           Pos.Infra.Util.JsonLog.Events (HasJsonLogConfig (..),
                     JsonLogConfig (..), MemPoolModifyReason, jsonLogDefault)
import           Pos.Launcher (HasConfigurations)
import           Pos.Util (postfixLFields)
import           Pos.Util.LoggerName (HasLoggerName' (..), askLoggerNameDefault,
                     modifyLoggerNameDefault)
import           Pos.Util.UserPublic (HasUserPublic (..), UserPublic)
import           Pos.Util.UserSecret (HasUserSecret (..), UserSecret)
import           Pos.Util.Util (HasLens (..))
import           Pos.Util.Wlog (HasLoggerName (..), LoggerName)
import           Pos.Wallet.Redirect (applyLastUpdateWebWallet,
                     blockchainSlotDurationWebWallet, connectedPeersWebWallet,
                     localChainDifficultyWebWallet,
                     networkChainDifficultyWebWallet, txpNormalizeWebWallet,
                     txpProcessTxWebWallet, waitForUpdateWebWallet)
import qualified System.Metrics as Metrics

import           Pos.Wallet.WalletMode (MonadBlockchainInfo (..),
                     MonadUpdates (..), WalletMempoolExt)
import           Pos.Wallet.Web.ClientTypes (AccountId)
import           Pos.Wallet.Web.Mode (getBalanceDefault, getNewAddressWebWallet,
                     getOwnUtxosDefault)
import           Pos.Wallet.Web.State (WalletDB, openMemState)
import           Pos.Wallet.Web.Tracking.BListener (onApplyBlocksWebWallet,
                     onRollbackBlocksWebWallet)
import           Pos.Wallet.Web.Tracking.Types (SyncQueue)

import           Test.Pos.Block.Logic.Emulation (Emulation (..), runEmulation)
import           Test.Pos.Block.Logic.Mode (BlockTestContext (..),
                     BlockTestContextTag, HasTestParams (..), TestParams (..),
                     btcSystemStartL, btcTxpMemL,
                     currentTimeSlottingTestDefault,
                     getCurrentSlotBlockingTestDefault,
                     getCurrentSlotInaccurateTestDefault,
                     getCurrentSlotTestDefault, initBlockTestContext)
import           Test.Pos.Core.Dummy (dummyConfig, dummyEpochSlots)

----------------------------------------------------------------------------
-- Parameters
----------------------------------------------------------------------------

-- | This datatype contains all parameters which should be generated before
-- testing starts
data WalletTestParams = WalletTestParams
    { _wtpBlockTestParams :: !TestParams
    -- ^ Block test params
    -- TODO add wallet parameters
    }

makeClassy ''WalletTestParams

instance Arbitrary WalletTestParams where
    arbitrary = WalletTestParams <$> arbitrary

instance Buildable WalletTestParams where
    build WalletTestParams {..} =
        bprint ("WalletTestParams {\n"%
                "  blockTestParams = "%build%"\n"%
                "}\n")
        _wtpBlockTestParams

instance Show WalletTestParams where
    show = formatToString build

----------------------------------------------------------------------------
-- Wallet context
----------------------------------------------------------------------------

data WalletTestContext = WalletTestContext
    { wtcBlockTestContext :: !BlockTestContext
    , wtcWalletState      :: !WalletDB
    , wtcUserPublic       :: !(TVar UserPublic)
    -- ^ Public keys which are used to identify external wallets
    , wtcUserSecret       :: !(TVar UserSecret)
    -- ^ Secret keys which are used to send transactions
    , wtcRecoveryHeader   :: !RecoveryHeader
    -- ^ Stub empty value, not used for tests for now.
    , wtcLastKnownHeader  :: !LastKnownHeader
    , wtcStateLock        :: !StateLock
    -- ^ A lock which manages access to shared resources.
    -- Stored hash is a hash of last applied block.
    , wtcStateLockMetrics :: !(StateLockMetrics MemPoolModifyReason)
    -- ^ A set of callbacks for 'StateLock'.
    , wtcShutdownContext  :: !ShutdownContext
    -- ^ Stub
    , wtcConnectedPeers   :: !ConnectedPeers
    -- ^ Stub
    , wtcSentTxs          :: !(TVar [TxAux])
    -- ^ Sent transactions via MonadWalletSendActions
    , wtcSyncQueue        :: !SyncQueue
    -- ^ STM queue for wallet sync requests.
    , wtcSlottingStateVar :: SimpleSlottingStateVar
    -- ^ A mutable cell with SlotId
    }

makeLensesWith postfixLFields ''WalletTestContext

type WalletTestMode = ReaderT WalletTestContext Emulation

getSentTxs :: WalletTestMode [TxAux]
getSentTxs = atomically . readTVar =<< view wtcSentTxs_L

----------------------------------------------------------------------------
-- Initialization
----------------------------------------------------------------------------

initWalletTestContext
    :: HasDlgConfiguration
    => WalletTestParams
    -> (WalletTestContext -> Emulation a)
    -> Emulation a
initWalletTestContext WalletTestParams {..} callback =
    initBlockTestContext dummyConfig _wtpBlockTestParams
        $ \wtcBlockTestContext -> do
            wtc <- liftIO $ do
                wtcWalletState <- openMemState
                wtcUserPublic <- STM.newTVarIO def
                wtcUserSecret <- STM.newTVarIO def
                wtcRecoveryHeader <- STM.newEmptyTMVarIO
                -- some kind of kostil to get tip
                tip <- readTVarIO $ txpTip $ btcTxpMem wtcBlockTestContext
                wtcStateLock <- newStateLock tip
                store <- liftIO $ Metrics.newStore
                wtcStateLockMetrics <- liftIO $ recordTxpMetrics
                    store
                    (txpMemPool $ btcTxpMem wtcBlockTestContext)
                wtcShutdownContext <- ShutdownContext <$> STM.newTVarIO False
                wtcConnectedPeers <- ConnectedPeers <$> STM.newTVarIO mempty
                wtcLastKnownHeader <- STM.newTVarIO Nothing
                wtcSentTxs <- STM.newTVarIO mempty
                wtcSyncQueue <- STM.newTQueueIO
                wtcSlottingStateVar <- mkSimpleSlottingStateVar dummyEpochSlots
                pure WalletTestContext {..}
            callback wtc

runWalletTestMode
    :: HasDlgConfiguration => WalletTestParams -> WalletTestMode a -> IO a
runWalletTestMode wtp action =
    runEmulation (getTimestamp $ wtp ^. wtpBlockTestParams . tpStartTime) $
    initWalletTestContext wtp $
    runReaderT action

----------------------------------------------------------------------------
-- Property
----------------------------------------------------------------------------

-- WalletProperty is similar to BlockProperty from Test.Pos.Block.Logic.Mode.
type WalletProperty = PropertyM WalletTestMode

-- | Convert 'WalletProperty' to 'Property' using given generator of
-- 'WalletTestParams'.
walletPropertyToProperty
    :: (HasDlgConfiguration, Testable a)
    => Gen WalletTestParams
    -> WalletProperty a
    -> Property
walletPropertyToProperty wtpGen walletProperty =
    forAll wtpGen $ \wtp ->
        monadic (ioProperty . runWalletTestMode wtp) walletProperty

instance (HasDlgConfiguration, Testable a) => Testable (WalletProperty a) where
    property = walletPropertyToProperty arbitrary

walletPropertySpec ::
       (HasDlgConfiguration, Testable a)
    => String
    -> WalletProperty a
    -> Spec
walletPropertySpec description wp =
    prop description (walletPropertyToProperty arbitrary wp)

----------------------------------------------------------------------------
-- Instances derived from BlockTestContext
----------------------------------------------------------------------------

instance HasLens BlockTestContextTag WalletTestContext BlockTestContext where
    lensOf = wtcBlockTestContext_L

instance HasLens SyncQueue WalletTestContext SyncQueue where
    lensOf = wtcSyncQueue_L

instance HasAllSecrets WalletTestContext where
    allSecrets = wtcBlockTestContext_L . allSecrets

instance HasLens DBPureVar WalletTestContext DBPureVar where
    lensOf = wtcBlockTestContext_L . lensOf @DBPureVar

instance GS.HasGStateContext WalletTestContext where
    gStateContext = wtcBlockTestContext_L . GS.gStateContext

instance HasSlottingVar WalletTestContext where
    slottingTimestamp = wtcBlockTestContext_L . btcSystemStartL
    slottingVar = wtcBlockTestContext_L . GS.gStateContext . GS.gscSlottingVar

instance HasSlogGState WalletTestContext where
    slogGState = wtcBlockTestContext_L . slogGState

instance HasLens LrcContext WalletTestContext LrcContext where
    lensOf = wtcBlockTestContext_L . lensOf @LrcContext

instance HasLens TxpGlobalSettings WalletTestContext TxpGlobalSettings where
    lensOf = wtcBlockTestContext_L . lensOf @TxpGlobalSettings

instance HasLens DelegationVar WalletTestContext DelegationVar where
    lensOf = wtcBlockTestContext_L . lensOf @DelegationVar

instance HasLens SscMemTag WalletTestContext SscState where
    lensOf = wtcBlockTestContext_L . lensOf @SscMemTag

instance MonadSlotsData ctx WalletTestMode => MonadSlots ctx WalletTestMode where
    getCurrentSlot _ = getCurrentSlotTestDefault
    getCurrentSlotBlocking _ = getCurrentSlotBlockingTestDefault
    getCurrentSlotInaccurate _ = getCurrentSlotInaccurateTestDefault
    currentTimeSlotting = currentTimeSlottingTestDefault

instance HasUserPublic WalletTestContext where
    userPublic = wtcUserPublic_L

instance HasUserSecret WalletTestContext where
    userSecret = wtcUserSecret_L

instance HasLens UpdateContext WalletTestContext UpdateContext where
      lensOf = wtcBlockTestContext_L . lensOf @UpdateContext

instance HasJsonLogConfig WalletTestContext where
    jsonLogConfig = lens (const JsonLogDisabled) const

instance {-# OVERLAPPING #-} CanJsonLog WalletTestMode where
    jsonLog = jsonLogDefault

instance HasLoggerName' WalletTestContext where
    loggerName = wtcBlockTestContext_L . lensOf @LoggerName

instance HasLens TxpHolderTag WalletTestContext (GenericTxpLocalData WalletMempoolExt) where
    lensOf = wtcBlockTestContext_L . btcTxpMemL

instance HasLens SimpleSlottingStateVar WalletTestContext SimpleSlottingStateVar where
    lensOf = wtcSlottingStateVar_L

instance {-# OVERLAPPING #-} HasLoggerName WalletTestMode where
    askLoggerName = askLoggerNameDefault
    modifyLoggerName = modifyLoggerNameDefault

instance MonadDBRead WalletTestMode where
    dbGet = DB.dbGetPureDefault
    dbIterSource = DB.dbIterSourcePureDefault
    dbGetSerBlock = const DB.dbGetSerBlockPureDefault
    dbGetSerUndo = const DB.dbGetSerUndoPureDefault
    dbGetSerBlund = const DB.dbGetSerBlundPureDefault

instance MonadDB WalletTestMode where
    dbPut = DB.dbPutPureDefault
    dbWriteBatch = DB.dbWriteBatchPureDefault
    dbDelete = DB.dbDeletePureDefault
    dbPutSerBlunds = DB.dbPutSerBlundsPureDefault

instance MonadGState WalletTestMode where
    gsAdoptedBVData = gsAdoptedBVDataDefault

----------------------------------------------------------------------------
-- Wallet instances
----------------------------------------------------------------------------

instance HasLens WalletDB WalletTestContext WalletDB where
    lensOf = wtcWalletState_L

-- For MonadUpdates
instance HasShutdownContext WalletTestContext where
    shutdownContext = wtcShutdownContext_L

-- For MonadRecoveryInfo
instance HasLens RecoveryHeaderTag WalletTestContext RecoveryHeader where
    lensOf = wtcRecoveryHeader_L

-- For BListener
instance HasLens StateLock WalletTestContext StateLock where
    lensOf = wtcStateLock_L

-- For MonadBlockchainInfo
instance HasLens LastKnownHeaderTag WalletTestContext LastKnownHeader where
    lensOf = wtcLastKnownHeader_L

instance HasLens ConnectedPeers WalletTestContext ConnectedPeers where
    lensOf = wtcConnectedPeers_L

-- For reporting
instance HasNodeType WalletTestContext where
    getNodeType _ = NodeCore -- doesn't really matter, it's for reporting

-- TODO may be used for callback on tx processing in future.
instance HasLens (StateLockMetrics MemPoolModifyReason) WalletTestContext (StateLockMetrics MemPoolModifyReason) where
    lensOf = wtcStateLockMetrics_L

-- This never made any sense. WalletDbReader is a type synonym.
-- instance WalletDbReader WalletTestContext WalletTestMode

instance MonadAddresses WalletTestMode where
    type AddrData WalletTestMode = (AccountId, PassPhrase)
    getNewAddress _ = getNewAddressWebWallet
    getFakeChangeAddress _ = pure largestHDAddressBoot

instance MonadKeysRead WalletTestMode where
    getPublic = getPublicDefault
    getSecret = getSecretDefault

instance MonadKeys WalletTestMode where
    modifyPublic = modifyPublicPureDefault
    modifySecret = modifySecretPureDefault

instance MonadTxHistory WalletTestMode where
    getBlockHistory = getBlockHistoryDefault
    getLocalHistory = getLocalHistoryDefault
    saveTx = saveTxDefault

instance MonadBalances WalletTestMode where
    getOwnUtxos = const $ getOwnUtxosDefault
    getBalance = const $ getBalanceDefault

instance MonadUpdates WalletTestMode where
    waitForUpdate = waitForUpdateWebWallet
    applyLastUpdate = applyLastUpdateWebWallet

instance MonadBListener WalletTestMode where
    onApplyBlocks = onApplyBlocksWebWallet
    onRollbackBlocks = onRollbackBlocksWebWallet

instance MonadBlockchainInfo WalletTestMode where
    networkChainDifficulty = networkChainDifficultyWebWallet
    localChainDifficulty = localChainDifficultyWebWallet
    blockchainSlotDuration = blockchainSlotDurationWebWallet
    connectedPeers = connectedPeersWebWallet

type instance MempoolExt WalletTestMode = WalletMempoolExt

instance (HasConfigurations)
        => MonadTxpLocal (BlockGenMode WalletMempoolExt WalletTestMode) where
    txpNormalize = txNormalize
    txpProcessTx = txProcessTransactionNoLock


instance MonadTxpLocal WalletTestMode where
    txpNormalize = txpNormalizeWebWallet
    txpProcessTx = txpProcessTxWebWallet

submitTxTestMode :: TxAux -> WalletTestMode Bool
submitTxTestMode txAux = True <$ (asks wtcSentTxs >>= atomically . flip STM.modifyTVar (txAux:))

instance MonadReporting WalletTestMode where
    report = const (pure ())
