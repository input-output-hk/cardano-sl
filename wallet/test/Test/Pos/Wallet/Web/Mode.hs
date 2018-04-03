{-# LANGUAGE Rank2Types      #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
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
import qualified Data.Text.Buildable
import           Formatting (bprint, build, formatToString, (%))
import qualified Prelude
import           System.Wlog (HasLoggerName (..), LoggerName)
import           Test.Hspec (Spec)
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck (Arbitrary (..), Property, Testable (..), forAll, ioProperty)
import           Test.QuickCheck.Gen (Gen)
import           Test.QuickCheck.Monadic (PropertyM (..), monadic)

import           Pos.AllSecrets (HasAllSecrets (..))
import           Pos.Block.BListener (MonadBListener (..))
import           Pos.Block.Slog (HasSlogGState (..))
import           Pos.Block.Types (LastKnownHeader, LastKnownHeaderTag, RecoveryHeader,
                                  RecoveryHeaderTag)
import           Pos.Client.KeyStorage (MonadKeys (..), MonadKeysRead (..), getSecretDefault,
                                        modifySecretPureDefault)
import           Pos.Client.Txp.Addresses (MonadAddresses (..))
import           Pos.Client.Txp.Balances (MonadBalances (..))
import           Pos.Client.Txp.History (MonadTxHistory (..), getBlockHistoryDefault,
                                         getLocalHistoryDefault, saveTxDefault)
import           Pos.Configuration (HasNodeConfiguration)
import           Pos.Context (ConnectedPeers (..))
import           Pos.Core (HasConfiguration, Timestamp (..), largestHDAddressBoot)
import           Pos.Core.Txp (TxAux)
import           Pos.Crypto (PassPhrase)
import           Pos.DB (MonadDB (..), MonadDBRead (..), MonadGState (..))
import qualified Pos.DB as DB
import qualified Pos.DB.Block as DB
import           Pos.DB.DB (gsAdoptedBVDataDefault)
import           Pos.DB.Pure (DBPureVar)
import           Pos.Delegation (DelegationVar, HasDlgConfiguration)
import           Pos.Generator.Block (BlockGenMode)
import qualified Pos.GState as GS
import           Pos.KnownPeers (MonadFormatPeers (..), MonadKnownPeers (..))
import           Pos.Launcher (HasConfigurations)
import           Pos.Lrc (LrcContext)
import           Pos.Network.Types (HasNodeType (..), NodeType (..))
import           Pos.Reporting (HasReportingContext (..))
import           Pos.Shutdown (HasShutdownContext (..), ShutdownContext (..))
import           Pos.Slotting (HasSlottingVar (..), MonadSlots (..), MonadSlotsData,
                               SimpleSlottingStateVar, mkSimpleSlottingStateVar)
import           Pos.Ssc.Configuration (HasSscConfiguration)
import           Pos.Ssc.Mem (SscMemTag)
import           Pos.Ssc.Types (SscState)
import           Pos.StateLock (StateLock, StateLockMetrics (..), newStateLock)
import           Pos.Txp (GenericTxpLocalData, MempoolExt, MonadTxpLocal (..), TxpGlobalSettings,
                          TxpHolderTag, recordTxpMetrics, txNormalize, txProcessTransactionNoLock,
                          txpMemPool, txpTip)
import           Pos.Update.Context (UpdateContext)
import           Pos.Util (postfixLFields)
import           Pos.Util.CompileInfo (HasCompileInfo)
import           Pos.Util.JsonLog.Events (HasJsonLogConfig (..), JsonLogConfig (..),
                                          MemPoolModifyReason, jsonLogDefault)
import           Pos.Util.LoggerName (HasLoggerName' (..), askLoggerNameDefault,
                                      modifyLoggerNameDefault)
import           Pos.Util.TimeWarp (CanJsonLog (..))
import           Pos.Util.UserSecret (HasUserSecret (..), UserSecret)
import           Pos.Util.Util (HasLens (..))
import           Pos.Wallet.Redirect (applyLastUpdateWebWallet, blockchainSlotDurationWebWallet,
                                      connectedPeersWebWallet, localChainDifficultyWebWallet,
                                      networkChainDifficultyWebWallet, txpNormalizeWebWallet,
                                      txpProcessTxWebWallet, waitForUpdateWebWallet)
import qualified System.Metrics as Metrics

import           Pos.Wallet.WalletMode (MonadBlockchainInfo (..), MonadUpdates (..),
                                        WalletMempoolExt)
import           Pos.Wallet.Web.ClientTypes (AccountId)
import           Pos.Wallet.Web.Mode (getBalanceDefault, getNewAddressWebWallet, getOwnUtxosDefault)
import           Pos.Wallet.Web.State (WalletDB, openMemState)
import           Pos.Wallet.Web.Tracking.BListener (onApplyBlocksWebWallet,
                                                    onRollbackBlocksWebWallet)
import           Pos.Wallet.Web.Tracking.Types (SyncQueue)

import           Test.Pos.Block.Logic.Emulation (Emulation (..), runEmulation)
import           Test.Pos.Block.Logic.Mode (BlockTestContext (..), BlockTestContextTag,
                                            HasTestParams (..), TestParams (..),
                                            btcReportingContextL, btcSystemStartL, btcTxpMemL,
                                            currentTimeSlottingTestDefault,
                                            getCurrentSlotBlockingTestDefault,
                                            getCurrentSlotInaccurateTestDefault,
                                            getCurrentSlotTestDefault, initBlockTestContext)

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

initWalletTestContext ::
       ( HasConfiguration
       , HasSscConfiguration
       , HasDlgConfiguration
       , HasNodeConfiguration
       )
    => WalletTestParams
    -> (WalletTestContext -> Emulation a)
    -> Emulation a
initWalletTestContext WalletTestParams {..} callback =
    initBlockTestContext _wtpBlockTestParams $ \wtcBlockTestContext -> do
        wtc <- liftIO $ do
            wtcWalletState <- openMemState
            wtcUserSecret <- STM.newTVarIO def
            wtcRecoveryHeader <- STM.newEmptyTMVarIO
            -- some kind of kostil to get tip
            tip <- readTVarIO $ txpTip $ btcTxpMem wtcBlockTestContext
            wtcStateLock <- newStateLock tip
            store <- liftIO $ Metrics.newStore
            wtcStateLockMetrics <- liftIO $ recordTxpMetrics store (txpMemPool $ btcTxpMem wtcBlockTestContext)
            wtcShutdownContext <- ShutdownContext <$> STM.newTVarIO False
            wtcConnectedPeers <- ConnectedPeers <$> STM.newTVarIO mempty
            wtcLastKnownHeader <- STM.newTVarIO Nothing
            wtcSentTxs <- STM.newTVarIO mempty
            wtcSyncQueue <- STM.newTQueueIO
            wtcSlottingStateVar <- mkSimpleSlottingStateVar
            pure WalletTestContext {..}
        callback wtc

runWalletTestMode ::
       ( HasConfiguration
       , HasSscConfiguration
       , HasDlgConfiguration
       , HasNodeConfiguration
       )
    => WalletTestParams
    -> WalletTestMode a
    -> IO a
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
    :: (HasConfiguration, HasSscConfiguration, HasDlgConfiguration, HasNodeConfiguration, Testable a)
    => Gen WalletTestParams
    -> WalletProperty a
    -> Property
walletPropertyToProperty wtpGen walletProperty =
    forAll wtpGen $ \wtp ->
        monadic (ioProperty . runWalletTestMode wtp) walletProperty

instance (HasConfiguration, HasSscConfiguration, HasDlgConfiguration, HasNodeConfiguration, Testable a)
        => Testable (WalletProperty a) where
    property = walletPropertyToProperty arbitrary

walletPropertySpec ::
       (HasConfiguration, HasSscConfiguration, HasDlgConfiguration, HasNodeConfiguration, Testable a)
    => String
    -> (HasConfiguration => WalletProperty a)
    -> Spec
walletPropertySpec description wp = prop description (walletPropertyToProperty arbitrary wp)

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

instance (HasConfiguration, MonadSlotsData ctx WalletTestMode)
       => MonadSlots ctx WalletTestMode where
    getCurrentSlot = getCurrentSlotTestDefault
    getCurrentSlotBlocking = getCurrentSlotBlockingTestDefault
    getCurrentSlotInaccurate = getCurrentSlotInaccurateTestDefault
    currentTimeSlotting = currentTimeSlottingTestDefault

instance HasUserSecret WalletTestContext where
    userSecret = wtcUserSecret_L

instance HasLens UpdateContext WalletTestContext UpdateContext where
      lensOf = wtcBlockTestContext_L . lensOf @UpdateContext

instance HasReportingContext WalletTestContext where
    reportingContext = wtcBlockTestContext_L . btcReportingContextL

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

instance HasConfiguration => MonadDBRead WalletTestMode where
    dbGet = DB.dbGetPureDefault
    dbIterSource = DB.dbIterSourcePureDefault
    dbGetSerBlock = DB.dbGetSerBlockPureDefault
    dbGetSerUndo = DB.dbGetSerUndoPureDefault

instance HasConfiguration => MonadDB WalletTestMode where
    dbPut = DB.dbPutPureDefault
    dbWriteBatch = DB.dbWriteBatchPureDefault
    dbDelete = DB.dbDeletePureDefault
    dbPutSerBlunds = DB.dbPutSerBlundsPureDefault

instance HasConfiguration => MonadGState WalletTestMode where
    gsAdoptedBVData = gsAdoptedBVDataDefault

instance MonadFormatPeers WalletTestMode where
    formatKnownPeers _ = pure Nothing

instance MonadKnownPeers WalletTestMode where
    updatePeersBucket _ _ = pure True

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
-- instance HasConfigurations => WalletDbReader WalletTestContext WalletTestMode

-- TODO remove HasCompileInfo here
-- when getNewAddressWebWallet won't require MonadWalletWebMode
instance HasConfigurations => MonadAddresses WalletTestMode where
    type AddrData WalletTestMode = (AccountId, PassPhrase)
    getNewAddress = getNewAddressWebWallet
    getFakeChangeAddress = pure largestHDAddressBoot

instance MonadKeysRead WalletTestMode where
    getSecret = getSecretDefault

instance MonadKeys WalletTestMode where
    modifySecret = modifySecretPureDefault

instance (HasCompileInfo, HasConfigurations) => MonadTxHistory WalletTestMode where
    getBlockHistory = getBlockHistoryDefault
    getLocalHistory = getLocalHistoryDefault
    saveTx = saveTxDefault

instance HasConfiguration => MonadBalances WalletTestMode where
    getOwnUtxos = getOwnUtxosDefault
    getBalance = getBalanceDefault

instance MonadUpdates WalletTestMode where
    waitForUpdate = waitForUpdateWebWallet
    applyLastUpdate = applyLastUpdateWebWallet

instance (HasCompileInfo, HasConfigurations) => MonadBListener WalletTestMode where
    onApplyBlocks = onApplyBlocksWebWallet
    onRollbackBlocks = onRollbackBlocksWebWallet

instance HasConfiguration => MonadBlockchainInfo WalletTestMode where
    networkChainDifficulty = networkChainDifficultyWebWallet
    localChainDifficulty = localChainDifficultyWebWallet
    blockchainSlotDuration = blockchainSlotDurationWebWallet
    connectedPeers = connectedPeersWebWallet

type instance MempoolExt WalletTestMode = WalletMempoolExt

instance (HasCompileInfo, HasConfigurations)
        => MonadTxpLocal (BlockGenMode WalletMempoolExt WalletTestMode) where
    txpNormalize = txNormalize
    txpProcessTx = txProcessTransactionNoLock


instance (HasCompileInfo, HasConfigurations) => MonadTxpLocal WalletTestMode where
    txpNormalize = txpNormalizeWebWallet
    txpProcessTx = txpProcessTxWebWallet

submitTxTestMode :: TxAux -> WalletTestMode Bool
submitTxTestMode txAux = True <$ (asks wtcSentTxs >>= atomically . flip STM.modifyTVar (txAux:))
