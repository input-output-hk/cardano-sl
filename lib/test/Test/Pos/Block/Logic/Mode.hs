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
       , blockPropertyTestable
       , blockPropertySpec
       ) where

import           Universum

import           Control.Lens                   (lens, makeClassy, makeLensesWith)
import           Data.Default                   (def)
import qualified Data.Map                       as Map
import qualified Data.Text.Buildable
import           Data.Time.Units                (TimeUnit (..))
import           Ether.Internal                 (HasLens (..))
import           Formatting                     (bprint, build, formatToString, shown,
                                                 (%))
import           Mockable                       (Production, currentTime, runProduction)
import qualified Prelude
import           System.Wlog                    (HasLoggerName (..), LoggerName)
import           Test.Hspec                     (Spec)
import           Test.Hspec.QuickCheck          (prop)
import           Test.QuickCheck                (Arbitrary (..), Gen, Property, forAll,
                                                 ioProperty)
import           Test.QuickCheck.Monadic        (PropertyM, monadic)

import           Pos.AllSecrets                 (AllSecrets (..), HasAllSecrets (..),
                                                 mkAllSecretsSimple)
import           Pos.Block.BListener            (MonadBListener (..), onApplyBlocksStub,
                                                 onRollbackBlocksStub)
import           Pos.Block.Core                 (Block, BlockHeader)
import           Pos.Block.Slog                 (HasSlogGState (..), mkSlogGState)
import           Pos.Block.Types                (Undo)
import           Pos.Configuration              (HasNodeConfiguration)
import           Pos.Core                       (BlockVersionData, CoreConfiguration (..),
                                                 GenesisConfiguration (..),
                                                 GenesisInitializer (..),
                                                 GenesisSpec (..), HasConfiguration,
                                                 IsHeader, SlotId,
                                                 TestnetDistribution (..), Timestamp (..),
                                                 genesisSecretKeys, withGenesisSpec)
import           Pos.Core.Configuration         (HasGenesisBlockVersionData,
                                                 withGenesisBlockVersionData)
import           Pos.DB                         (DBPure, MonadBlockDBGeneric (..),
                                                 MonadBlockDBGenericWrite (..),
                                                 MonadDB (..), MonadDBRead (..),
                                                 MonadGState (..))
import qualified Pos.DB                         as DB
import qualified Pos.DB.Block                   as DB
import           Pos.DB.DB                      (gsAdoptedBVDataDefault, initNodeDBs)
import           Pos.DB.Pure                    (DBPureVar, newDBPureVar)
import           Pos.Delegation                 (DelegationVar, mkDelegationVar)
import           Pos.Generator.Block            (BlockGenMode)
import           Pos.Generator.BlockEvent       (SnapshotId)
import qualified Pos.GState                     as GS
import           Pos.KnownPeers                 (MonadFormatPeers (..))
import           Pos.Launcher.Configuration     (Configuration (..), HasConfigurations)
import           Pos.Lrc                        (LrcContext (..), mkLrcSyncData)
import           Pos.Network.Types              (HasNodeType (..), NodeType (..))
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
import           Pos.Ssc.Extra                  (SscMemTag, SscState, mkSscState)
import           Pos.Ssc.GodTossing             (HasGtConfiguration,)
import           Pos.Txp                        (GenericTxpLocalData, MempoolExt,
                                                 MonadTxpLocal (..), TxpGlobalSettings,
                                                 TxpHolderTag, mkTxpLocalData,
                                                 txNormalize, txProcessTransactionNoLock,
                                                 txpGlobalSettings)
import           Pos.Update.Context             (UpdateContext, mkUpdateContext)
import           Pos.Util                       (Some, newInitFuture, postfixLFields)
import           Pos.Util.CompileInfo           (withCompileInfo)
import           Pos.Util.LoggerName            (HasLoggerName' (..),
                                                 getLoggerNameDefault,
                                                 modifyLoggerNameDefault)
import           Pos.WorkMode                   (EmptyMempoolExt)

import           Test.Pos.Block.Logic.Emulation (Emulation (..), runEmulation, sudoLiftIO)
import           Test.Pos.Configuration         (defaultTestBlockVersionData,
                                                 defaultTestConf, defaultTestGenesisSpec)

----------------------------------------------------------------------------
-- Parameters
----------------------------------------------------------------------------

-- | This data type contains all parameters which should be generated
-- before testing starts.
data TestParams = TestParams
    { _tpStartTime          :: !Timestamp
    -- ^ System start time.
    , _tpBlockVersionData   :: !BlockVersionData
    -- ^ Genesis 'BlockVersionData' in tests.
    , _tpGenesisInitializer :: !GenesisInitializer
    -- ^ 'GenesisInitializer' in 'TestParams' allows one to use custom
    -- genesis data.
    }

makeClassy ''TestParams

instance Buildable TestParams where
    build TestParams {..} =
        bprint ("TestParams {\n"%
                "  start time: "%shown%"\n"%
                "  initializer: "%build%"\n"%
                "}\n")
            _tpStartTime
            _tpGenesisInitializer

instance Show TestParams where
    show = formatToString build

instance Arbitrary TestParams where
    arbitrary = do
        let _tpStartTime = Timestamp (fromMicroseconds 0)
        let _tpBlockVersionData = defaultTestBlockVersionData
        _tpGenesisInitializer <-
            withGenesisBlockVersionData
                _tpBlockVersionData
                genGenesisInitializer
        return TestParams {..}

genGenesisInitializer :: HasGenesisBlockVersionData => Gen GenesisInitializer
genGenesisInitializer = do
    tiTestBalance <- arbitrary
    tiFakeAvvmBalance <- arbitrary
    let tiDistribution = TestnetRichmenStakeDistr
    tiSeed <- arbitrary
    return TestnetInitializer {..}

-- This function creates 'CoreConfiguration' from 'TestParams' and
-- uses it to satisfy 'HasConfiguration'.
withTestParams :: TestParams -> (HasConfiguration => r) -> r
withTestParams TestParams {..} = withGenesisSpec _tpStartTime coreConfiguration
  where
    defaultCoreConf :: CoreConfiguration
    defaultCoreConf = ccCore defaultTestConf
    coreConfiguration :: CoreConfiguration
    coreConfiguration = defaultCoreConf {ccGenesis = GCSpec genesisSpec}
    genesisSpec =
        defaultTestGenesisSpec
        { gsInitializer = _tpGenesisInitializer
        , gsBlockVersionData = _tpBlockVersionData
        }

----------------------------------------------------------------------------
-- Init mode with instances
----------------------------------------------------------------------------

-- The fields are lazy on purpose: this allows using them with
-- futures.
data TestInitModeContext = TestInitModeContext
    { timcDBPureVar   :: DBPureVar
    , timcSlottingVar :: TVar SlottingData
    , timcSystemStart :: !Timestamp
    , timcLrcContext  :: LrcContext
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
    , btcSscState          :: !SscState
    , btcTxpMem            :: !(GenericTxpLocalData EmptyMempoolExt)
    , btcTxpGlobalSettings :: !TxpGlobalSettings
    , btcSlotId            :: !(Maybe SlotId)
    -- ^ If this value is 'Just' we will return it as the current
    -- slot. Otherwise simple slotting is used.
    , btcParams            :: !TestParams
    , btcReportingContext  :: !ReportingContext
    , btcDelegation        :: !DelegationVar
    , btcPureDBSnapshots   :: !PureDBSnapshotsVar
    , btcAllSecrets        :: !AllSecrets
    }

makeLensesWith postfixLFields ''BlockTestContext

instance HasTestParams BlockTestContext where
    testParams = btcParams_L

instance HasAllSecrets BlockTestContext where
    allSecrets = btcAllSecrets_L

----------------------------------------------------------------------------
-- Initialization
----------------------------------------------------------------------------

initBlockTestContext
    :: (HasConfiguration, HasGtConfiguration, HasNodeConfiguration)
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
                futureSlottingVar
                systemStart
                futureLrcCtx
        initBlockTestContextDo = do
            initNodeDBs
            _gscSlottingVar <- newTVarIO =<< GS.getSlottingData
            putSlottingVar _gscSlottingVar
            btcSSlottingVar <- mkSimpleSlottingVar
            let btcLoggerName = "testing"
            lcLrcSync <- mkLrcSyncData >>= newTVarIO
            let _gscLrcContext = LrcContext {..}
            putLrcCtx _gscLrcContext
            btcUpdateContext <- mkUpdateContext
            btcSscState <- mkSscState
            _gscSlogGState <- mkSlogGState
            btcTxpMem <- mkTxpLocalData
            let btcTxpGlobalSettings = txpGlobalSettings
            let btcReportingContext = emptyReportingContext
            let btcSlotId = Nothing
            let btcParams = tp
            let btcGState = GS.GStateContext {_gscDB = DB.PureDB dbPureVar, ..}
            btcDelegation <- mkDelegationVar
            btcPureDBSnapshots <- PureDBSnapshotsVar <$> newIORef Map.empty
            let secretKeys =
                    case genesisSecretKeys of
                        Nothing ->
                            error "initBlockTestContext: no genesisSecretKeys"
                        Just ks -> ks
            let btcAllSecrets = mkAllSecretsSimple secretKeys
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

runBlockTestMode :: (HasNodeConfiguration, HasGtConfiguration, HasConfiguration)
                 => TestParams -> BlockTestMode a -> IO a
runBlockTestMode tp action =
    runEmulation (getTimestamp $ tp ^. tpStartTime) $
    initBlockTestContext tp (runReaderT action)

----------------------------------------------------------------------------
-- Property
----------------------------------------------------------------------------

type BlockProperty = PropertyM BlockTestMode

-- | Convert 'BlockProperty' to 'Property' using given generator of
-- 'TestParams'.
blockPropertyToProperty ::
       (HasNodeConfiguration, HasGtConfiguration)
    => Gen TestParams
    -> (HasConfiguration =>
            BlockProperty a)
    -> Property
blockPropertyToProperty tpGen blockProperty =
    forAll tpGen $ \tp ->
        withTestParams tp $
        monadic (ioProperty . runBlockTestMode tp) blockProperty

-- | Simplified version of 'blockPropertyToProperty' which uses
-- 'Arbitrary' instance to generate 'TestParams'.
--
-- You can treat it as 'Testable' instance for 'HasConfiguration =>
-- BlockProperty a', but unfortunately it's impossible to write such
-- instance.
--
-- The following code doesn't compile:
--
-- instance (HasNodeConfiguration, HasGtConfiguration)
--          => Testable (HasConfiguration => BlockProperty a) where
--     property = blockPropertyToProperty arbitrary
blockPropertyTestable ::
       (HasNodeConfiguration, HasGtConfiguration)
    => (HasConfiguration =>
            BlockProperty a)
    -> Property
blockPropertyTestable = blockPropertyToProperty arbitrary

-- | Specialized version of 'prop' function from 'hspec'.
blockPropertySpec ::
       (HasNodeConfiguration, HasGtConfiguration)
    => String
    -> (HasConfiguration =>
            BlockProperty a)
    -> Spec
blockPropertySpec description bp = prop description (blockPropertyTestable bp)

----------------------------------------------------------------------------
-- Boilerplate TestInitContext instances
----------------------------------------------------------------------------

instance HasLens DBPureVar TestInitModeContext DBPureVar where
    lensOf = timcDBPureVar_L

instance HasLens LrcContext TestInitModeContext LrcContext where
    lensOf = timcLrcContext_L

instance HasSlottingVar TestInitModeContext where
    slottingTimestamp = timcSystemStart_L
    slottingVar = timcSlottingVar_L

instance HasConfiguration => MonadDBRead TestInitMode where
    dbGet = DB.dbGetPureDefault
    dbIterSource = DB.dbIterSourcePureDefault

instance HasConfiguration => MonadDB TestInitMode where
    dbPut = DB.dbPutPureDefault
    dbWriteBatch = DB.dbWriteBatchPureDefault
    dbDelete = DB.dbDeletePureDefault

instance
    HasConfiguration =>
    MonadBlockDBGeneric BlockHeader Block Undo TestInitMode
  where
    dbGetBlock  = DB.dbGetBlockPureDefault
    dbGetUndo   = DB.dbGetUndoPureDefault
    dbGetHeader = DB.dbGetHeaderPureDefault

instance
    HasConfiguration =>
    MonadBlockDBGenericWrite BlockHeader Block Undo TestInitMode
  where
    dbPutBlund = DB.dbPutBlundPureDefault

instance
    HasConfiguration =>
    MonadBlockDBGeneric (Some IsHeader) SscBlock () TestInitMode
  where
    dbGetBlock  = DB.dbGetBlockSscPureDefault
    dbGetUndo   = DB.dbGetUndoSscPureDefault
    dbGetHeader = DB.dbGetHeaderSscPureDefault

instance (HasConfiguration, MonadSlotsData ctx TestInitMode)
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

instance HasLens SscMemTag BlockTestContext SscState where
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

instance HasLens TxpHolderTag BlockTestContext (GenericTxpLocalData EmptyMempoolExt) where
    lensOf = btcTxpMem_L

instance HasLoggerName' BlockTestContext where
    loggerName = lensOf @LoggerName

instance HasNodeType BlockTestContext where
    getNodeType _ = NodeCore -- doesn't really matter, it's for reporting

instance {-# OVERLAPPING #-} HasLoggerName BlockTestMode where
    getLoggerName = getLoggerNameDefault
    modifyLoggerName = modifyLoggerNameDefault

instance (HasConfiguration, MonadSlotsData ctx BlockTestMode)
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

instance HasConfiguration => MonadDBRead BlockTestMode where
    dbGet = DB.dbGetPureDefault
    dbIterSource = DB.dbIterSourcePureDefault

instance HasConfiguration => MonadDB BlockTestMode where
    dbPut = DB.dbPutPureDefault
    dbWriteBatch = DB.dbWriteBatchPureDefault
    dbDelete = DB.dbDeletePureDefault

instance HasConfiguration =>
         MonadBlockDBGeneric BlockHeader Block Undo BlockTestMode
  where
    dbGetBlock = DB.dbGetBlockPureDefault
    dbGetUndo = DB.dbGetUndoPureDefault
    dbGetHeader = DB.dbGetHeaderPureDefault

instance HasConfiguration => MonadBlockDBGeneric (Some IsHeader) SscBlock () BlockTestMode
  where
    dbGetBlock = DB.dbGetBlockSscPureDefault
    dbGetUndo = DB.dbGetUndoSscPureDefault
    dbGetHeader = DB.dbGetHeaderSscPureDefault

instance HasConfiguration =>
         MonadBlockDBGenericWrite BlockHeader Block Undo BlockTestMode where
    dbPutBlund = DB.dbPutBlundPureDefault

instance HasConfiguration => MonadGState BlockTestMode where
    gsAdoptedBVData = gsAdoptedBVDataDefault

instance MonadBListener BlockTestMode where
    onApplyBlocks = onApplyBlocksStub
    onRollbackBlocks = onRollbackBlocksStub

instance MonadFormatPeers BlockTestMode where
    formatKnownPeers _ = pure Nothing

type instance MempoolExt BlockTestMode = EmptyMempoolExt

instance HasConfigurations => MonadTxpLocal (BlockGenMode EmptyMempoolExt BlockTestMode) where
    txpNormalize = withCompileInfo def $ txNormalize
    txpProcessTx = withCompileInfo def $ txProcessTransactionNoLock

instance HasConfigurations => MonadTxpLocal BlockTestMode where
    txpNormalize = withCompileInfo def $ txNormalize
    txpProcessTx = withCompileInfo def $ txProcessTransactionNoLock
