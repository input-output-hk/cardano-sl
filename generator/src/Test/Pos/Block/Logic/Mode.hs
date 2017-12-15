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
       , BlockTestMode
       , runBlockTestMode
       , initBlockTestContext

       , BlockProperty
       , blockPropertyToProperty
       , blockPropertyTestable

       -- Lens
       , btcGStateL
       , btcSystemStartL
       , btcLoggerNameL
       , btcSSlottingVarL
       , btcUpdateContextL
       , btcSscStateL
       , btcTxpMemL
       , btcTxpGlobalSettingsL
       , btcSlotIdL
       , btcParamsL
       , btcReportingContextL
       , btcDelegationL
       , btcPureDBSnapshotsL
       , btcAllSecretsL

       -- MonadSlots
       , getCurrentSlotTestDefault
       , getCurrentSlotBlockingTestDefault
       , getCurrentSlotInaccurateTestDefault
       , currentTimeSlottingTestDefault
       ) where

import           Universum

import           Control.Lens (lens, makeClassy, makeLensesWith)
import           Data.Default (def)
import qualified Data.Map as Map
import qualified Data.Text.Buildable
import           Data.Time.Units (TimeUnit (..))
import           Ether.Internal (HasLens (..))
import           Formatting (bprint, build, formatToString, shown, (%))
import           Mockable (Production, currentTime, runProduction)
import qualified Prelude
import           System.Wlog (HasLoggerName (..), LoggerName)
import           Test.QuickCheck (Arbitrary (..), Gen, Property, forAll, ioProperty)
import           Test.QuickCheck.Monadic (PropertyM, monadic)

import           Pos.AllSecrets (AllSecrets (..), HasAllSecrets (..), mkAllSecretsSimple)
import           Pos.Block.BListener (MonadBListener (..), onApplyBlocksStub, onRollbackBlocksStub)
import           Pos.Block.Slog (HasSlogGState (..), mkSlogGState)
import           Pos.Communication.Limits (HasAdoptedBlockVersionData (..))
import           Pos.Configuration (HasNodeConfiguration)
import           Pos.Core (BlockVersionData, CoreConfiguration (..), GenesisConfiguration (..),
                           GenesisInitializer (..), GenesisSpec (..), HasConfiguration, SlotId,
                           Timestamp (..), genesisSecretKeys, withGenesisSpec)
import           Pos.Core.Configuration (HasGenesisBlockVersionData, withGenesisBlockVersionData)
import           Pos.DB (DBPure, MonadDB (..), MonadDBRead (..), MonadGState (..))
import qualified Pos.DB as DB
import qualified Pos.DB.Block as DB
import           Pos.DB.DB (gsAdoptedBVDataDefault, initNodeDBs)
import           Pos.DB.Pure (DBPureVar, newDBPureVar)
import           Pos.Delegation (DelegationVar, HasDlgConfiguration, mkDelegationVar)
import           Pos.Generator.Block (BlockGenMode)
import           Pos.Generator.BlockEvent (SnapshotId)
import qualified Pos.GState as GS
import           Pos.KnownPeers (MonadFormatPeers (..))
import           Pos.Launcher.Configuration (Configuration (..), HasConfigurations)
import           Pos.Lrc (LrcContext (..), mkLrcSyncData)
import           Pos.Network.Types (HasNodeType (..), NodeType (..))
import           Pos.Reporting (HasReportingContext (..), ReportingContext, emptyReportingContext)
import           Pos.Slotting (HasSlottingVar (..), MonadSlots (..), SimpleSlottingMode,
                               SimpleSlottingVar, currentTimeSlottingSimple,
                               getCurrentSlotBlockingSimple, getCurrentSlotInaccurateSimple,
                               getCurrentSlotSimple, mkSimpleSlottingVar)
import           Pos.Slotting.MemState (MonadSlotsData)
import           Pos.Slotting.Types (SlottingData)
import           Pos.Ssc (HasSscConfiguration, SscMemTag, SscState, mkSscState)
import           Pos.Txp (GenericTxpLocalData, MempoolExt, MonadTxpLocal (..), TxpGlobalSettings,
                          TxpHolderTag, mkTxpLocalData, txNormalize, txProcessTransactionNoLock,
                          txpGlobalSettings)
import           Pos.Update.Context (UpdateContext, mkUpdateContext)
import           Pos.Util (newInitFuture, postfixLFields, postfixLFields2)
import           Pos.Util.CompileInfo (withCompileInfo)
import           Pos.Util.LoggerName (HasLoggerName' (..), getLoggerNameDefault,
                                      modifyLoggerNameDefault)
import           Pos.WorkMode (EmptyMempoolExt)

import           Test.Pos.Block.Logic.Emulation (Emulation (..), runEmulation, sudoLiftIO)
import           Test.Pos.Configuration (defaultTestBlockVersionData, defaultTestConf,
                                         defaultTestGenesisSpec)

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
    giTestBalance <- arbitrary
    giFakeAvvmBalance <- arbitrary
    giAvvmBalanceFactor <- arbitrary
    giUseHeavyDlg <- arbitrary
    giSeed <- arbitrary
    return GenesisInitializer {..}

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


makeLensesWith postfixLFields2 ''BlockTestContext

instance HasTestParams BlockTestContext where
    testParams = btcParamsL

instance HasAllSecrets BlockTestContext where
    allSecrets = btcAllSecretsL

----------------------------------------------------------------------------
-- Initialization
----------------------------------------------------------------------------

initBlockTestContext ::
       ( HasConfiguration
       , HasSscConfiguration
       , HasDlgConfiguration
       , HasNodeConfiguration
       )
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

runBlockTestMode ::
       ( HasNodeConfiguration
       , HasSscConfiguration
       , HasDlgConfiguration
       , HasConfiguration
       )
    => TestParams
    -> BlockTestMode a
    -> IO a
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
       (HasNodeConfiguration, HasDlgConfiguration, HasSscConfiguration)
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
-- instance (HasNodeConfiguration, HasSscConfiguration)
--          => Testable (HasConfiguration => BlockProperty a) where
--     property = blockPropertyToProperty arbitrary
blockPropertyTestable ::
       (HasNodeConfiguration, HasDlgConfiguration, HasSscConfiguration)
    => (HasConfiguration => BlockProperty a)
    -> Property
blockPropertyTestable = blockPropertyToProperty arbitrary

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
    dbGetSerBlock = DB.dbGetSerBlockPureDefault
    dbGetSerUndo = DB.dbGetSerUndoPureDefault

instance HasConfiguration => MonadDB TestInitMode where
    dbPut = DB.dbPutPureDefault
    dbWriteBatch = DB.dbWriteBatchPureDefault
    dbDelete = DB.dbDeletePureDefault
    dbPutSerBlund = DB.dbPutSerBlundPureDefault

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
    gStateContext = btcGStateL

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
    lensOf = btcPureDBSnapshotsL

instance HasLens LoggerName BlockTestContext LoggerName where
      lensOf = btcLoggerNameL

instance HasLens LrcContext BlockTestContext LrcContext where
    lensOf = GS.gStateContext . GS.gscLrcContext

instance HasLens UpdateContext BlockTestContext UpdateContext where
      lensOf = btcUpdateContextL

instance HasLens SscMemTag BlockTestContext SscState where
      lensOf = btcSscStateL

instance HasLens TxpGlobalSettings BlockTestContext TxpGlobalSettings where
      lensOf = btcTxpGlobalSettingsL

instance HasLens TestParams BlockTestContext TestParams where
      lensOf = btcParamsL

instance HasLens SimpleSlottingVar BlockTestContext SimpleSlottingVar where
      lensOf = btcSSlottingVarL

instance HasReportingContext BlockTestContext where
    reportingContext = btcReportingContextL

instance HasSlottingVar BlockTestContext where
    slottingTimestamp = btcSystemStartL
    slottingVar = GS.gStateContext . GS.gscSlottingVar

instance HasSlogGState BlockTestContext where
    slogGState = GS.gStateContext . GS.gscSlogGState

instance HasLens DelegationVar BlockTestContext DelegationVar where
    lensOf = btcDelegationL

instance HasLens TxpHolderTag BlockTestContext (GenericTxpLocalData EmptyMempoolExt) where
    lensOf = btcTxpMemL

instance HasLoggerName' BlockTestContext where
    loggerName = lensOf @LoggerName

instance HasNodeType BlockTestContext where
    getNodeType _ = NodeCore -- doesn't really matter, it's for reporting

instance {-# OVERLAPPING #-} HasLoggerName BlockTestMode where
    getLoggerName = getLoggerNameDefault
    modifyLoggerName = modifyLoggerNameDefault

type TestSlottingContext ctx m =
    ( SimpleSlottingMode ctx m
    , HasLens BlockTestContextTag ctx BlockTestContext
    )

testSlottingHelper
    :: TestSlottingContext ctx m
    => (SimpleSlottingVar -> m a)
    -> (SlotId -> a)
    -> m a
testSlottingHelper targetF alternative = do
    BlockTestContext{..} <- view (lensOf @BlockTestContextTag)
    case btcSlotId of
        Nothing   -> targetF btcSSlottingVar
        Just slot -> pure $ alternative slot

getCurrentSlotTestDefault :: TestSlottingContext ctx m => m (Maybe SlotId)
getCurrentSlotTestDefault = testSlottingHelper getCurrentSlotSimple Just

getCurrentSlotBlockingTestDefault :: TestSlottingContext ctx m => m SlotId
getCurrentSlotBlockingTestDefault = testSlottingHelper getCurrentSlotBlockingSimple identity

getCurrentSlotInaccurateTestDefault :: TestSlottingContext ctx m => m SlotId
getCurrentSlotInaccurateTestDefault = testSlottingHelper getCurrentSlotInaccurateSimple identity

currentTimeSlottingTestDefault :: SimpleSlottingMode ctx m => m Timestamp
currentTimeSlottingTestDefault = currentTimeSlottingSimple

instance (HasConfiguration, MonadSlotsData ctx BlockTestMode)
        => MonadSlots ctx BlockTestMode where
    getCurrentSlot = getCurrentSlotTestDefault
    getCurrentSlotBlocking = getCurrentSlotBlockingTestDefault
    getCurrentSlotInaccurate = getCurrentSlotInaccurateTestDefault
    currentTimeSlotting = currentTimeSlottingTestDefault

instance HasConfiguration => MonadDBRead BlockTestMode where
    dbGet = DB.dbGetPureDefault
    dbIterSource = DB.dbIterSourcePureDefault
    dbGetSerBlock = DB.dbGetSerBlockPureDefault
    dbGetSerUndo = DB.dbGetSerUndoPureDefault

instance HasConfiguration => MonadDB BlockTestMode where
    dbPut = DB.dbPutPureDefault
    dbWriteBatch = DB.dbWriteBatchPureDefault
    dbDelete = DB.dbDeletePureDefault
    dbPutSerBlund = DB.dbPutSerBlundPureDefault

instance HasConfiguration => MonadGState BlockTestMode where
    gsAdoptedBVData = gsAdoptedBVDataDefault

instance HasConfiguration => HasAdoptedBlockVersionData BlockTestMode where
    adoptedBVData = gsAdoptedBVData

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
