{-# LANGUAGE CPP             #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE TypeOperators   #-}

-- | Execution modes for block logic tests.

module Test.Pos.Block.Logic.Mode
       ( TestParams (..)
       , HasTestParams (..)
       , TestInitModeContext (..)
       , BlockTestContextTag
       , PureDBSnapshotsVar(..)
       , BlockTestContext(..)
       , btcSlotId_L
       , BlockTestMode
       , runBlockTestMode

       , BlockProperty
       , blockPropertyToProperty
       , genSuitableBalanceDistribution
       ) where

import           Universum

import           Control.Lens                   (lens, makeClassy, makeLensesWith)
import qualified Data.Map                       as Map
import qualified Data.Text.Buildable
import           Data.Time.Units                (Microsecond, TimeUnit (..))
import           Ether.Internal                 (HasLens (..))
import           Formatting                     (bprint, build, formatToString, shown,
                                                 (%))
import           Mockable                       (Production, currentTime, runProduction)
import qualified Prelude
import           System.Wlog                    (HasLoggerName (..), LoggerName)
import           Test.QuickCheck                (Arbitrary (..), Gen, Property,
                                                 Testable (..), choose, forAll,
                                                 ioProperty, oneof, suchThat)
import           Test.QuickCheck.Monadic        (PropertyM, monadic)

import           Pos.AllSecrets                 (AllSecrets (..), HasAllSecrets (..),
                                                 mkInvAddrSpendingData, mkInvSecretsMap)
import           Pos.Block.BListener            (MonadBListener (..), onApplyBlocksStub,
                                                 onRollbackBlocksStub)
import           Pos.Block.Core                 (Block, BlockHeader)
import           Pos.Block.Slog                 (HasSlogGState (..), mkSlogGState)
import           Pos.Block.Types                (Undo)
import           Pos.Core                       (AddrSpendingData (..),
                                                 BalanceDistribution (..),
                                                 HasCoreConstants, IsHeader, SlotId,
                                                 Timestamp (..), makePubKeyAddressBoot,
                                                 mkCoin, unsafeGetCoin)
import           Pos.Crypto                     (SecretKey, toPublic)
import           Pos.DB                         (DBPure, MonadBlockDBGeneric (..),
                                                 MonadBlockDBGenericWrite (..),
                                                 MonadDB (..), MonadDBRead (..),
                                                 MonadGState (..))
import qualified Pos.DB                         as DB
import qualified Pos.DB.Block                   as DB
import           Pos.DB.DB                      (gsAdoptedBVDataDefault, initNodeDBs)
import           Pos.DB.Pure                    (DBPureVar, newDBPureVar)
import           Pos.Delegation                 (DelegationVar, mkDelegationVar)
import           Pos.Generator.BlockEvent       (SnapshotId)
import           Pos.Genesis                    (GenesisContext (..), GenesisUtxo (..),
                                                 GenesisWStakeholders (..),
                                                 genesisContextImplicit, gtcUtxo,
                                                 gtcWStakeholders, safeExpBalances)
import qualified Pos.GState                     as GS
import           Pos.KnownPeers                 (MonadFormatPeers (..))
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
import           Pos.Slotting.MemState          (MonadSlotsData)
import           Pos.Ssc.Class                  (SscBlock)
import           Pos.Ssc.Class.Helpers          (SscHelpersClass)
import           Pos.Ssc.Extra                  (SscMemTag, SscState, mkSscState)
import           Pos.Ssc.GodTossing             (SscGodTossing)
import           Pos.Txp                        (GenericTxpLocalData, TxpGlobalSettings,
                                                 TxpHolderTag, mkTxpLocalData, utxoF)
import           Pos.Update.Context             (UpdateContext, mkUpdateContext)
import           Pos.Util                       (Some, newInitFuture, postfixLFields)
import           Pos.Util.LoggerName            (HasLoggerName' (..),
                                                 getLoggerNameDefault,
                                                 modifyLoggerNameDefault)
import           Pos.WorkMode.Class             (TxpExtra_TMP)
#ifdef WITH_EXPLORER
import           Pos.Explorer                   (explorerTxpGlobalSettings)
#else
import           Pos.Txp                        (txpGlobalSettings)
#endif

import           Test.Pos.Block.Logic.Emulation (Emulation (..), runEmulation, sudoLiftIO)

-- Remove this once there's no #ifdef-ed Pos.Txp import
{-# ANN module ("HLint: ignore Use fewer imports" :: Text) #-}

----------------------------------------------------------------------------
-- Parameters
----------------------------------------------------------------------------

-- | This data type contains all parameters which should be generated
-- before testing starts.
data TestParams = TestParams
    { _tpGenesisContext       :: !GenesisContext
    , _tpAllSecrets           :: !AllSecrets
    -- ^ Secret keys corresponding to 'PubKeyAddress'es from
    -- genesis 'Utxo'.
    -- They are stored in map (with 'StakeholderId' as key) to make it easy
    -- to find 'SecretKey' corresponding to given 'StakeholderId'.
    -- In tests we often want to have inverse of 'hash' and 'toPublic'.
    , _tpBalanceDistributions :: ![BalanceDistribution]
    -- ^ Balance distributions which were used to generate genesis txp data.
    -- It's primarily needed to see (in logs) which distribution was used (e. g.
    -- when test fails).
    , _tpStartTime            :: !Microsecond
    }

makeClassy ''TestParams

instance HasAllSecrets TestParams where
    allSecrets = tpAllSecrets

instance Buildable TestParams where
    build TestParams {..} =
        bprint ("TestParams {\n"%
                "  utxo = "%utxoF%"\n"%
                "  secrets: "%build%"\n"%
                "  balance distributions: "%shown%"\n"%
                "  start time: "%shown%"\n"%
                "}\n")
            utxo
            _tpAllSecrets
            _tpBalanceDistributions
            _tpStartTime
      where
        utxo = unGenesisUtxo (_tpGenesisContext ^. gtcUtxo)

instance Show TestParams where
    show = formatToString build

-- More distributions can be added if we want (e. g. RichPoor).
genSuitableBalanceDistribution :: Word -> Gen BalanceDistribution
genSuitableBalanceDistribution stakeholdersNum =
    oneof [ genFlat
          {-, genBitcoin-} -- is broken
          , pure $ safeExpBalances (25::Integer) -- 25 participants should be enough
          ]
  where
    -- We set the lower bound to 10 ADA per stakeholder to make sure that we have
    -- enough money in genesis to generate transactions with proper fees
    totalCoins = mkCoin <$> choose ( fromIntegral stakeholdersNum * 10000000
                                   , unsafeGetCoin maxBound)
    genFlat = FlatBalances stakeholdersNum <$> totalCoins

instance Arbitrary TestParams where
    arbitrary = do
        secretKeysList <-
            toList @(NonEmpty SecretKey) <$>
             -- might have repetitions
            (arbitrary `suchThat` (\l -> length l < 15 && length l > 2))
        let invSecretsMap = mkInvSecretsMap secretKeysList
        let publicKeys = map toPublic (toList invSecretsMap)
        let addresses = map makePubKeyAddressBoot publicKeys
        let invAddrSpendingData =
                mkInvAddrSpendingData $
                addresses `zip` (map PubKeyASD publicKeys)
        balanceDistribution <-
            genSuitableBalanceDistribution (fromIntegral $ length invSecretsMap)
        let addrDistribution = [(addresses, balanceDistribution)]
        let _tpGenesisContext =
                genesisContextImplicit invAddrSpendingData addrDistribution
        let _tpAllSecrets = AllSecrets invSecretsMap invAddrSpendingData
        let _tpBalanceDistributions = one balanceDistribution
        let _tpStartTime = fromMicroseconds 0
        return TestParams {..}

----------------------------------------------------------------------------
-- Init mode with instances
----------------------------------------------------------------------------

-- The fields are lazy on purpose: this allows using them with
-- futures.
data TestInitModeContext = TestInitModeContext
    { timcDBPureVar      :: DBPureVar
    , timcGenesisContext :: GenesisContext
    , timcSlottingVar    :: TVar SlottingData
    , timcSystemStart    :: !Timestamp
    , timcLrcContext     :: LrcContext
    }

makeLensesWith postfixLFields ''TestInitModeContext

type TestInitMode = ReaderT TestInitModeContext Production

runTestInitMode :: TestInitModeContext -> TestInitMode a -> IO a
runTestInitMode ctx = runProduction . flip runReaderT ctx

----------------------------------------------------------------------------
-- Main context
----------------------------------------------------------------------------

newtype PureDBSnapshotsVar = PureDBSnapshotsVar
    { getPureDBSnapshotsVar :: IORef (Map SnapshotId DBPure)
    }

data BlockTestContext = BlockTestContext
    { btcGState            :: !GS.GStateContext
    , btcSystemStart       :: !Timestamp
    , btcLoggerName        :: !LoggerName
    , btcSSlottingVar      :: !SimpleSlottingVar
    , btcUpdateContext     :: !UpdateContext
    , btcSscState          :: !(SscState SscGodTossing)
    , btcTxpMem            :: !(GenericTxpLocalData TxpExtra_TMP)
    , btcTxpGlobalSettings :: !TxpGlobalSettings
    , btcSlotId            :: !(Maybe SlotId)
    -- ^ If this value is 'Just' we will return it as the current
    -- slot. Otherwise simple slotting is used.
    , btcParams            :: !TestParams
    , btcReportingContext  :: !ReportingContext
    , btcDelegation        :: !DelegationVar
    , btcPureDBSnapshots   :: !PureDBSnapshotsVar
    }

makeLensesWith postfixLFields ''BlockTestContext

instance HasTestParams BlockTestContext where
    testParams = btcParams_L

instance HasAllSecrets BlockTestContext where
    allSecrets = testParams . allSecrets

----------------------------------------------------------------------------
-- Initialization
----------------------------------------------------------------------------

initBlockTestContext
    :: HasCoreConstants
    => TestParams
    -> (BlockTestContext -> Emulation a)
    -> Emulation a
initBlockTestContext tp@TestParams {..} callback = do
    clockVar <- Emulation ask
    dbPureVar <- newDBPureVar
    (futureLrcCtx, putLrcCtx) <- newInitFuture "lrcCtx"
    (futureSlottingVar, putSlottingVar) <- newInitFuture "slottingVar"
    systemStart <- Timestamp <$> currentTime
    let initCtx =
            TestInitModeContext
                dbPureVar
                _tpGenesisContext
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
            _gscSlogGState <- mkSlogGState
            btcTxpMem <- mkTxpLocalData
#ifdef WITH_EXPLORER
            let btcTxpGlobalSettings = explorerTxpGlobalSettings
#else
            let btcTxpGlobalSettings = txpGlobalSettings
#endif
            let btcReportingContext = emptyReportingContext
            let btcSlotId = Nothing
            let btcParams = tp
            let btcGState = GS.GStateContext {_gscDB = DB.PureDB dbPureVar, ..}
            btcDelegation <- mkDelegationVar @SscGodTossing
            btcPureDBSnapshots <- PureDBSnapshotsVar <$> newIORef Map.empty
            let btCtx = BlockTestContext {btcSystemStart = systemStart, ..}
            liftIO $ flip runReaderT clockVar $ unEmulation $ callback btCtx
    sudoLiftIO $ runTestInitMode initCtx $ initBlockTestContextDo

----------------------------------------------------------------------------
-- ExecMode
----------------------------------------------------------------------------

data BlockTestContextTag

instance HasLens BlockTestContextTag BlockTestContext BlockTestContext where
    lensOf = identity

type BlockTestMode = ReaderT BlockTestContext Emulation

runBlockTestMode :: HasCoreConstants => TestParams -> BlockTestMode a -> IO a
runBlockTestMode tp action =
    runEmulation (tp ^. tpStartTime) $
    initBlockTestContext tp (runReaderT action)

----------------------------------------------------------------------------
-- Property
----------------------------------------------------------------------------

type BlockProperty = PropertyM BlockTestMode

-- | Convert 'BlockProperty' to 'Property' using given generator of
-- 'TestParams'.
blockPropertyToProperty :: HasCoreConstants => Gen TestParams -> BlockProperty a -> Property
blockPropertyToProperty tpGen blockProperty =
    forAll tpGen $ \tp ->
        monadic (ioProperty . runBlockTestMode tp) blockProperty

-- | 'Testable' instance allows one to write monadic properties in
-- do-notation and pass them directly to QuickCheck engine. It uses
-- arbitrary 'TestParams'. For more fine-grained control over
-- parameters use 'blockPropertyToProperty'.
instance HasCoreConstants => Testable (BlockProperty a) where
    property = blockPropertyToProperty arbitrary

----------------------------------------------------------------------------
-- Boilerplate TestInitContext instances
----------------------------------------------------------------------------

instance HasLens DBPureVar TestInitModeContext DBPureVar where
    lensOf = timcDBPureVar_L

instance HasLens GenesisUtxo TestInitModeContext GenesisUtxo where
    lensOf = timcGenesisContext_L . gtcUtxo

instance HasLens GenesisWStakeholders TestInitModeContext GenesisWStakeholders where
    lensOf = timcGenesisContext_L . gtcWStakeholders

instance HasLens GenesisContext TestInitModeContext GenesisContext where
    lensOf = timcGenesisContext_L

instance HasLens LrcContext TestInitModeContext LrcContext where
    lensOf = timcLrcContext_L

instance HasSlottingVar TestInitModeContext where
    slottingTimestamp = timcSystemStart_L
    slottingVar = timcSlottingVar_L

instance MonadDBRead TestInitMode where
    dbGet = DB.dbGetPureDefault
    dbIterSource = DB.dbIterSourcePureDefault

instance MonadDB TestInitMode where
    dbPut = DB.dbPutPureDefault
    dbWriteBatch = DB.dbWriteBatchPureDefault
    dbDelete = DB.dbDeletePureDefault

instance
    (HasCoreConstants, SscHelpersClass ssc) =>
    MonadBlockDBGeneric (BlockHeader ssc) (Block ssc) Undo TestInitMode
  where
    dbGetBlock  = DB.dbGetBlockPureDefault @ssc
    dbGetUndo   = DB.dbGetUndoPureDefault @ssc
    dbGetHeader = DB.dbGetHeaderPureDefault @ssc

instance (HasCoreConstants, SscHelpersClass ssc) =>
         MonadBlockDBGenericWrite (BlockHeader ssc) (Block ssc) Undo TestInitMode where
    dbPutBlund = DB.dbPutBlundPureDefault

instance
    (HasCoreConstants, SscHelpersClass ssc) =>
    MonadBlockDBGeneric (Some IsHeader) (SscBlock ssc) () TestInitMode
  where
    dbGetBlock  = DB.dbGetBlockSscPureDefault @ssc
    dbGetUndo   = DB.dbGetUndoSscPureDefault @ssc
    dbGetHeader = DB.dbGetHeaderSscPureDefault @ssc

instance (HasCoreConstants, MonadSlotsData ctx TestInitMode)
      => MonadSlots ctx TestInitMode
  where
    getCurrentSlot           = getCurrentSlotSimple           =<< mkSimpleSlottingVar
    getCurrentSlotBlocking   = getCurrentSlotBlockingSimple   =<< mkSimpleSlottingVar
    getCurrentSlotInaccurate = getCurrentSlotInaccurateSimple =<< mkSimpleSlottingVar
    currentTimeSlotting      = currentTimeSlottingSimple

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

instance HasLens PureDBSnapshotsVar BlockTestContext PureDBSnapshotsVar where
    lensOf = btcPureDBSnapshots_L

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

instance HasSlottingVar BlockTestContext where
    slottingTimestamp = btcSystemStart_L
    slottingVar = GS.gStateContext . GS.gscSlottingVar

instance HasSlogGState BlockTestContext where
    slogGState = GS.gStateContext . GS.gscSlogGState

instance HasLens DelegationVar BlockTestContext DelegationVar where
    lensOf = btcDelegation_L

instance HasLens TxpHolderTag BlockTestContext (GenericTxpLocalData TxpExtra_TMP) where
    lensOf = btcTxpMem_L

instance HasLens GenesisUtxo BlockTestContext GenesisUtxo where
    lensOf = btcParams_L . tpGenesisContext . gtcUtxo

instance HasLens GenesisWStakeholders BlockTestContext GenesisWStakeholders where
    lensOf = btcParams_L . tpGenesisContext . gtcWStakeholders

instance HasLoggerName' BlockTestContext where
    loggerName = lensOf @LoggerName

instance {-# OVERLAPPING #-} HasLoggerName BlockTestMode where
    getLoggerName = getLoggerNameDefault
    modifyLoggerName = modifyLoggerNameDefault

instance (HasCoreConstants, MonadSlotsData ctx BlockTestMode)
      => MonadSlots ctx BlockTestMode
  where
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

instance HasCoreConstants =>
         MonadBlockDBGeneric (BlockHeader SscGodTossing) (Block SscGodTossing) Undo BlockTestMode
  where
    dbGetBlock = DB.dbGetBlockPureDefault
    dbGetUndo = DB.dbGetUndoPureDefault @SscGodTossing
    dbGetHeader = DB.dbGetHeaderPureDefault @SscGodTossing

instance HasCoreConstants => MonadBlockDBGeneric (Some IsHeader) (SscBlock SscGodTossing) () BlockTestMode
  where
    dbGetBlock = DB.dbGetBlockSscPureDefault
    dbGetUndo = DB.dbGetUndoSscPureDefault @SscGodTossing
    dbGetHeader = DB.dbGetHeaderSscPureDefault @SscGodTossing

instance HasCoreConstants =>
         MonadBlockDBGenericWrite (BlockHeader SscGodTossing) (Block SscGodTossing) Undo BlockTestMode where
    dbPutBlund = DB.dbPutBlundPureDefault

instance MonadGState BlockTestMode where
    gsAdoptedBVData = gsAdoptedBVDataDefault

instance MonadBListener BlockTestMode where
    onApplyBlocks = onApplyBlocksStub
    onRollbackBlocks = onRollbackBlocksStub

instance MonadFormatPeers BlockTestMode where
    formatKnownPeers _ = pure Nothing
