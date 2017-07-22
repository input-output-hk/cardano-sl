{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

-- | Execution modes for block logic tests.

module Test.Pos.Block.Logic.Mode
       ( TestParams (..)
       , HasTestParams (..)
       , TestInitModeContext (..)
       , BlockTestContextTag
       , BlockTestContext(..)
       , btcSlotId_L
       , BlockTestMode
       , runBlockTestMode

       , BlockProperty
       ) where

import           Universum

import           Control.Lens                   (lens, makeClassy, makeLensesWith)
import qualified Data.HashMap.Strict            as HM
import qualified Data.Map.Strict                as M
import qualified Data.Text.Buildable
import           Data.Time.Units                (Microsecond, TimeUnit (..))
import           Ether.Internal                 (HasLens (..))
import           Formatting                     (bprint, build, formatToString, shown,
                                                 (%))
import           Mockable                       (Production, currentTime, runProduction)
import qualified Prelude
import           System.Wlog                    (HasLoggerName (..), LoggerName)
import           Test.QuickCheck                (Arbitrary (..), Gen, Testable (..),
                                                 choose, ioProperty, oneof)
import           Test.QuickCheck.Monadic        (PropertyM, monadic)

import           Pos.Block.BListener            (MonadBListener (..), onApplyBlocksStub,
                                                 onRollbackBlocksStub)
import           Pos.Block.Core                 (Block, BlockHeader)
import           Pos.Block.Slog                 (HasSlogContext (..), mkSlogContext)
import           Pos.Block.Types                (Undo)
import           Pos.Core                       (IsHeader, SlotId, StakeDistribution (..),
                                                 Timestamp (..), addressHash,
                                                 makePubKeyAddress, mkCoin, unsafeGetCoin)
import           Pos.Crypto                     (SecretKey, toPublic, unsafeHash)
import           Pos.DB                         (MonadBlockDBGeneric (..),
                                                 MonadBlockDBGenericWrite (..),
                                                 MonadDB (..), MonadDBRead (..),
                                                 MonadGState (..))
import qualified Pos.DB                         as DB
import qualified Pos.DB.Block                   as DB
import           Pos.DB.DB                      (gsAdoptedBVDataDefault, initNodeDBs)
import           Pos.DB.Pure                    (DBPureVar, newDBPureVar)
import           Pos.Delegation                 (DelegationVar, mkDelegationVar)
import           Pos.Discovery                  (DiscoveryContextSum (..),
                                                 HasDiscoveryContextSum (..),
                                                 MonadDiscovery (..), findPeersSum,
                                                 getPeersSum)
import           Pos.Generator.Block            (AllSecrets (..), HasAllSecrets (..))
import           Pos.Genesis                    (stakeDistribution)
import qualified Pos.GState                     as GS
import           Pos.Launcher                   (newInitFuture)
import           Pos.Lrc                        (LrcContext (..), mkLrcSyncData)
import           Pos.Reporting                  (HasReportingContext (..),
                                                 ReportingContext, emptyReportingContext)
import           Pos.Slotting                   (HasSlottingVar (..), MonadSlots (..),
                                                 SimpleSlottingVar, SlottingData,
                                                 currentTimeSlottingSimple,
                                                 getCurrentSlotBlockingSimple,
                                                 getCurrentSlotInaccurateSimple,
                                                 getCurrentSlotSimple,
                                                 mkSimpleSlottingVar)
import           Pos.Slotting.MemState          (MonadSlotsData (..),
                                                 getSlottingDataDefault,
                                                 getSystemStartDefault,
                                                 putSlottingDataDefault,
                                                 waitPenultEpochEqualsDefault)
import           Pos.Ssc.Class                  (SscBlock)
import           Pos.Ssc.Class.Helpers          (SscHelpersClass)
import           Pos.Ssc.Extra                  (SscMemTag, SscState, mkSscState)
import           Pos.Ssc.GodTossing             (SscGodTossing)
import           Pos.Txp                        (GenericTxpLocalData, GenesisStakeholders,
                                                 GenesisTxpContext, GenesisUtxo (..),
                                                 TxIn (..), TxOut (..), TxOutAux (..),
                                                 TxpGlobalSettings, TxpHolderTag,
                                                 TxpMetrics, gtcStakeholders, gtcUtxo,
                                                 ignoreTxpMetrics, mkGenesisTxpContext,
                                                 mkTxpLocalData, txpGlobalSettings, utxoF)
import           Pos.Update.Context             (UpdateContext, mkUpdateContext)
import           Pos.Util.LoggerName            (HasLoggerName' (..),
                                                 getLoggerNameDefault,
                                                 modifyLoggerNameDefault)
import           Pos.Util.Util                  (Some, postfixLFields)
import           Pos.WorkMode.Class             (TxpExtra_TMP)

import           Test.Pos.Block.Logic.Emulation (Emulation (..), runEmulation, sudoLiftIO)

----------------------------------------------------------------------------
-- Parameters
----------------------------------------------------------------------------

-- | This data type contains all parameters which should be generated
-- before testing starts.
data TestParams = TestParams
    { _tpGenTxpContext     :: !GenesisTxpContext
    -- ^ Genesis 'Utxo'.
    , _tpAllSecrets        :: !AllSecrets
    -- ^ Secret keys corresponding to 'PubKeyAddress'es from
    -- genesis 'Utxo'.
    -- They are stored in map (with 'StakeholderId' as key) to make it easy
    -- to find 'SecretKey' corresponding to given 'StakeholderId'.
    -- In tests we often want to have inverse of 'hash' and 'toPublic'.
    , _tpStakeDistribution :: !StakeDistribution
    -- ^ Stake distribution which was used to generate genesis utxo.
    -- It's primarily needed to see which distribution was used (e. g.
    -- when test fails).
    , _tpStartTime         :: !Microsecond
    }

makeClassy ''TestParams

instance HasAllSecrets TestParams where
    allSecrets = tpAllSecrets

instance Buildable TestParams where
    build TestParams {..} =
        bprint ("TestParams {\n"%
                "  utxo = "%utxoF%"\n"%
                "  secrets: "%build%"\n"%
                "  stake distribution: "%shown%"\n"%
                "  start time: "%shown%"\n"%
                "}\n")
            utxo
            _tpAllSecrets
            _tpStakeDistribution
            _tpStartTime
      where
        utxo = _tpGenTxpContext & \g -> unGenesisUtxo (g ^. gtcUtxo)

instance Show TestParams where
    show = formatToString build

-- More distributions can be added if we want (e. g. RichPoor).
genSuitableStakeDistribution :: Word -> Gen StakeDistribution
genSuitableStakeDistribution stakeholdersNum =
    oneof [genFlat{-, genBitcoin-}, pure (ExponentialStakes stakeholdersNum)]
  where
    totalCoins = mkCoin <$> choose (fromIntegral stakeholdersNum, unsafeGetCoin maxBound)
    genFlat =
        FlatStakes stakeholdersNum <$> totalCoins
    -- Apparently bitcoin distribution is broken (it produces total stake
    -- greater than requested one) and I don't think it's worth fixing it.
    -- genBitcoin = BitcoinStakes stakeholdersNum <$> totalCoins

instance Arbitrary TestParams where
    arbitrary = do
        secretKeysList <- toList @(NonEmpty SecretKey) <$> arbitrary -- might have repetitions
        let _tpStartTime = fromMicroseconds 0
        let toSecretPair sk = (addressHash (toPublic sk), sk)
        let secretKeysMap = HM.fromList $ map toSecretPair secretKeysList
        let _tpAllSecrets = AllSecrets secretKeysMap
        _tpStakeDistribution <-
            genSuitableStakeDistribution (fromIntegral $ length secretKeysMap)
        let zipF secretKey (coin, toaDistr) =
                let addr = makePubKeyAddress (toPublic secretKey)
                    toaOut = TxOut addr coin
                in (TxIn (unsafeHash addr) 0, TxOutAux {..})
        let _tpGenTxpContext =
                mkGenesisTxpContext . M.fromList $
                zipWith
                    zipF
                    (toList secretKeysMap)
                    (stakeDistribution _tpStakeDistribution)
        return TestParams {..}

----------------------------------------------------------------------------
-- Init mode with instances
----------------------------------------------------------------------------

-- The fields are lazy on purpose: this allows using them with
-- futures.
data TestInitModeContext ssc = TestInitModeContext
    { timcDBPureVar   :: DBPureVar
    , timcGenesisUtxo :: GenesisUtxo
    , timcSlottingVar :: TVar SlottingData
    , timcSystemStart :: !Timestamp
    , timcLrcContext  :: LrcContext
    }

makeLensesWith postfixLFields ''TestInitModeContext

type TestInitMode ssc = ReaderT (TestInitModeContext ssc) Production

runTestInitMode :: TestInitModeContext ssc -> TestInitMode ssc a -> IO a
runTestInitMode ctx = runProduction . flip runReaderT ctx

----------------------------------------------------------------------------
-- Main context
----------------------------------------------------------------------------

data BlockTestContext = BlockTestContext
    { btcGState            :: !GS.GStateContext
    , btcSystemStart       :: !Timestamp
    , btcLoggerName        :: !LoggerName
    , btcSSlottingVar      :: !SimpleSlottingVar
    , btcUpdateContext     :: !UpdateContext
    , btcSscState          :: !(SscState SscGodTossing)
    , btcTxpMem            :: !(GenericTxpLocalData TxpExtra_TMP, TxpMetrics)
    , btcTxpGlobalSettings :: !TxpGlobalSettings
    , btcSlotId            :: !(Maybe SlotId)
    -- ^ If this value is 'Just' we will return it as the current
    -- slot. Otherwise simple slotting is used.
    , btcParams            :: !TestParams
    , btcReportingContext  :: !ReportingContext
    , btcDiscoveryContext  :: !DiscoveryContextSum
    , btcDelegation        :: !DelegationVar
    }

makeLensesWith postfixLFields ''BlockTestContext

instance HasTestParams BlockTestContext where
    testParams = btcParams_L

instance HasAllSecrets BlockTestContext where
    allSecrets = testParams . allSecrets

----------------------------------------------------------------------------
-- Initialization
----------------------------------------------------------------------------

initBlockTestContext ::
       TestParams -> (BlockTestContext -> Emulation a) -> Emulation a
initBlockTestContext tp@TestParams {..} callback = do
    clockVar <- Emulation ask
    dbPureVar <- newDBPureVar
    (futureLrcCtx, putLrcCtx) <- newInitFuture
    (futureSlottingVar, putSlottingVar) <- newInitFuture
    systemStart <- Timestamp <$> currentTime
    let initCtx =
            TestInitModeContext
                dbPureVar
                (_tpGenTxpContext ^. gtcUtxo)
                futureSlottingVar
                systemStart
                futureLrcCtx
        initBlockTestContextDo = do
            initNodeDBs @SscGodTossing
            _gscSlottingVar <- newTVarIO =<< GS.getSlottingData
            putSlottingVar _gscSlottingVar
            btcSSlottingVar <- mkSimpleSlottingVar
            let btcLoggerName = "testing"
            lcLrcSync <- mkLrcSyncData >>= newTVarIO
            let _gscLrcContext = LrcContext {..}
            putLrcCtx _gscLrcContext
            btcUpdateContext <- mkUpdateContext
            btcSscState <- mkSscState @SscGodTossing
            _gscSlogContext <- mkSlogContext
            btcTxpMem <- (, ignoreTxpMetrics) <$> mkTxpLocalData
            let btcTxpGlobalSettings = txpGlobalSettings
            let btcReportingContext = emptyReportingContext
            let btcDiscoveryContext = DCStatic mempty
            let btcSlotId = Nothing
            let btcParams = tp
            let btcGState = GS.GStateContext {_gscDB = DB.PureDB dbPureVar, ..}
            btcDelegation <- mkDelegationVar @SscGodTossing
            let btCtx = BlockTestContext {btcSystemStart = systemStart, ..}
            liftIO $ flip runReaderT clockVar $ unEmulation $ callback btCtx
    sudoLiftIO $ runTestInitMode @SscGodTossing initCtx $ initBlockTestContextDo

----------------------------------------------------------------------------
-- ExecMode
----------------------------------------------------------------------------

data BlockTestContextTag

instance HasLens BlockTestContextTag BlockTestContext BlockTestContext where
    lensOf = identity

type BlockTestMode = ReaderT BlockTestContext Emulation

runBlockTestMode :: TestParams -> BlockTestMode a -> IO a
runBlockTestMode tp action =
    runEmulation (tp ^. tpStartTime) $
    initBlockTestContext tp (runReaderT action)

----------------------------------------------------------------------------
-- Property
----------------------------------------------------------------------------

type BlockProperty = PropertyM BlockTestMode

instance Testable (BlockProperty a) where
    property blockProperty =
        property $ \testParams' ->
            (monadic (ioProperty . runBlockTestMode testParams') blockProperty)

----------------------------------------------------------------------------
-- Boilerplate TestInitContext instances
----------------------------------------------------------------------------

instance HasLens DBPureVar (TestInitModeContext ssc) DBPureVar where
    lensOf = timcDBPureVar_L

instance HasLens GenesisUtxo (TestInitModeContext ssc) GenesisUtxo where
    lensOf = timcGenesisUtxo_L

instance HasLens LrcContext (TestInitModeContext ssc) LrcContext where
    lensOf = timcLrcContext_L

instance HasSlottingVar (TestInitModeContext ssc) where
    slottingTimestamp = timcSystemStart_L
    slottingVar = timcSlottingVar_L

instance MonadDBRead (TestInitMode ssc) where
    dbGet = DB.dbGetPureDefault
    dbIterSource = DB.dbIterSourcePureDefault

instance MonadDB (TestInitMode ssc) where
    dbPut = DB.dbPutPureDefault
    dbWriteBatch = DB.dbWriteBatchPureDefault
    dbDelete = DB.dbDeletePureDefault

instance
    SscHelpersClass ssc =>
    MonadBlockDBGeneric (BlockHeader ssc) (Block ssc) Undo (TestInitMode ssc)
  where
    dbGetBlock  = DB.dbGetBlockPureDefault @ssc
    dbGetUndo   = DB.dbGetUndoPureDefault @ssc
    dbGetHeader = DB.dbGetHeaderPureDefault @ssc

instance SscHelpersClass ssc =>
         MonadBlockDBGenericWrite (BlockHeader ssc) (Block ssc) Undo (TestInitMode ssc) where
    dbPutBlund = DB.dbPutBlundPureDefault

instance
    SscHelpersClass ssc =>
    MonadBlockDBGeneric (Some IsHeader) (SscBlock ssc) () (TestInitMode ssc)
  where
    dbGetBlock  = DB.dbGetBlockSscPureDefault @ssc
    dbGetUndo   = DB.dbGetUndoSscPureDefault @ssc
    dbGetHeader = DB.dbGetHeaderSscPureDefault @ssc

instance MonadSlotsData (TestInitMode ssc) where
    getSystemStart = getSystemStartDefault
    getSlottingData = getSlottingDataDefault
    waitPenultEpochEquals = waitPenultEpochEqualsDefault
    putSlottingData = putSlottingDataDefault

instance MonadSlots (TestInitMode ssc) where
    getCurrentSlot = getCurrentSlotSimple =<< mkSimpleSlottingVar
    getCurrentSlotBlocking = getCurrentSlotBlockingSimple =<< mkSimpleSlottingVar
    getCurrentSlotInaccurate = getCurrentSlotInaccurateSimple =<< mkSimpleSlottingVar
    currentTimeSlotting = currentTimeSlottingSimple

----------------------------------------------------------------------------
-- Boilerplate BlockTestContext instances
----------------------------------------------------------------------------

instance GS.HasGStateContext BlockTestContext where
    gStateContext = btcGState_L

instance HasLens DBPureVar BlockTestContext DBPureVar where
    lensOf = GS.gStateContext . GS.gscDB . pureDBLens
      where
        -- pva701: sorry for newbie code
        getter = \case
            DB.RealDB _   -> realDBInTestsError
            DB.PureDB pdb -> pdb
        setter _ pdb = DB.PureDB pdb
        pureDBLens = lens getter setter
        realDBInTestsError = error "You are using real db in tests"

instance HasLens LoggerName BlockTestContext LoggerName where
      lensOf = btcLoggerName_L

instance HasLens LrcContext BlockTestContext LrcContext where
    lensOf = GS.gStateContext . GS.gscLrcContext

instance HasLens UpdateContext BlockTestContext UpdateContext where
      lensOf = btcUpdateContext_L

instance HasLens SscMemTag BlockTestContext (SscState SscGodTossing) where
      lensOf = btcSscState_L

instance HasLens TxpGlobalSettings BlockTestContext TxpGlobalSettings where
      lensOf = btcTxpGlobalSettings_L

instance HasLens TestParams BlockTestContext TestParams where
      lensOf = btcParams_L

instance HasLens SimpleSlottingVar BlockTestContext SimpleSlottingVar where
      lensOf = btcSSlottingVar_L

instance HasReportingContext BlockTestContext where
    reportingContext = btcReportingContext_L

instance HasDiscoveryContextSum BlockTestContext where
    discoveryContextSum = btcDiscoveryContext_L

instance HasSlottingVar BlockTestContext where
    slottingTimestamp = btcSystemStart_L
    slottingVar = GS.gStateContext . GS.gscSlottingVar

instance HasSlogContext BlockTestContext where
    slogContextL = GS.gStateContext . GS.gscSlogContext

instance HasLens DelegationVar BlockTestContext DelegationVar where
    lensOf = btcDelegation_L

instance HasLens TxpHolderTag BlockTestContext (GenericTxpLocalData TxpExtra_TMP, TxpMetrics) where
    lensOf = btcTxpMem_L

instance HasLens GenesisUtxo BlockTestContext GenesisUtxo where
    lensOf = btcParams_L . tpGenTxpContext . gtcUtxo

instance HasLens GenesisStakeholders BlockTestContext GenesisStakeholders where
    lensOf = btcParams_L . tpGenTxpContext . gtcStakeholders

instance HasLoggerName' BlockTestContext where
    loggerName = lensOf @LoggerName

instance {-# OVERLAPPING #-} HasLoggerName BlockTestMode where
    getLoggerName = getLoggerNameDefault
    modifyLoggerName = modifyLoggerNameDefault

instance MonadSlotsData BlockTestMode where
    getSystemStart = getSystemStartDefault
    getSlottingData = getSlottingDataDefault
    waitPenultEpochEquals = waitPenultEpochEqualsDefault
    putSlottingData = putSlottingDataDefault

instance MonadSlots BlockTestMode where
    getCurrentSlot = do
        view btcSlotId_L >>= \case
            Nothing -> getCurrentSlotSimple =<< view btcSSlottingVar_L
            Just slot -> pure (Just slot)
    getCurrentSlotBlocking =
        view btcSlotId_L >>= \case
            Nothing -> getCurrentSlotBlockingSimple =<< view btcSSlottingVar_L
            Just slot -> pure slot
    getCurrentSlotInaccurate =
        view btcSlotId_L >>= \case
            Nothing -> getCurrentSlotInaccurateSimple =<< view btcSSlottingVar_L
            Just slot -> pure slot
    currentTimeSlotting = currentTimeSlottingSimple

instance MonadDBRead BlockTestMode where
    dbGet = DB.dbGetPureDefault
    dbIterSource = DB.dbIterSourcePureDefault

instance MonadDB BlockTestMode where
    dbPut = DB.dbPutPureDefault
    dbWriteBatch = DB.dbWriteBatchPureDefault
    dbDelete = DB.dbDeletePureDefault

instance MonadBlockDBGeneric (BlockHeader SscGodTossing) (Block SscGodTossing) Undo BlockTestMode
  where
    dbGetBlock = DB.dbGetBlockPureDefault
    dbGetUndo = DB.dbGetUndoPureDefault @SscGodTossing
    dbGetHeader = DB.dbGetHeaderPureDefault @SscGodTossing

instance MonadBlockDBGeneric (Some IsHeader) (SscBlock SscGodTossing) () BlockTestMode
  where
    dbGetBlock = DB.dbGetBlockSscPureDefault
    dbGetUndo = DB.dbGetUndoSscPureDefault @SscGodTossing
    dbGetHeader = DB.dbGetHeaderSscPureDefault @SscGodTossing

instance MonadBlockDBGenericWrite (BlockHeader SscGodTossing) (Block SscGodTossing) Undo BlockTestMode where
    dbPutBlund = DB.dbPutBlundPureDefault

instance MonadGState BlockTestMode where
    gsAdoptedBVData = gsAdoptedBVDataDefault

instance MonadBListener BlockTestMode where
    onApplyBlocks = onApplyBlocksStub
    onRollbackBlocks = onRollbackBlocksStub

instance MonadDiscovery BlockTestMode where
    getPeers = getPeersSum
    findPeers = findPeersSum
