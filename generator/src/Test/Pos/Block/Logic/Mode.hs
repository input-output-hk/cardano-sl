{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}
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
       , btcSSlottingStateVarL
       , btcUpdateContextL
       , btcSscStateL
       , btcTxpMemL
       , btcTxpGlobalSettingsL
       , btcSlotIdL
       , btcParamsL
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
import qualified Data.Map as Map
import qualified Data.Set as Set
import           Data.Time.Units (TimeUnit (..))
import           Formatting (bprint, build, formatToString, shown, (%))
import qualified Formatting.Buildable
import qualified Prelude
import           Test.QuickCheck (Arbitrary (..), Gen, Property, forAll,
                     ioProperty)
import           Test.QuickCheck.Monadic (PropertyM, monadic)
import           Test.QuickCheck.Property (Testable)

import           Pos.AllSecrets (AllSecrets (..), HasAllSecrets (..),
                     mkAllSecretsSimple)
import           Pos.Chain.Block (HasSlogGState (..))
import           Pos.Chain.Delegation (DelegationVar, HasDlgConfiguration)
import           Pos.Chain.Ssc (SscMemTag, SscState)
import           Pos.Chain.Txp (TxpConfiguration (..))
import           Pos.Core as Core (Config (..), SlotId, Timestamp (..),
                     configEpochSlots, configGeneratedSecretsThrow, mkConfig)
import           Pos.Core.Conc (currentTime)
import           Pos.Core.Genesis (GenesisInitializer (..), GenesisSpec (..),
                     gsSecretKeys)
import           Pos.Core.Reporting (HasMisbehaviorMetrics (..),
                     MonadReporting (..))
import           Pos.Core.Slotting (MonadSlotsData)
import           Pos.Core.Update (BlockVersionData)
import           Pos.DB (DBPure, MonadDB (..), MonadDBRead (..),
                     MonadGState (..))
import qualified Pos.DB as DB
import           Pos.DB.Block (MonadBListener (..), mkSlogGState,
                     onApplyBlocksStub, onRollbackBlocksStub)
import qualified Pos.DB.Block as DB
import           Pos.DB.DB (gsAdoptedBVDataDefault, initNodeDBs)
import           Pos.DB.Delegation (mkDelegationVar)
import           Pos.DB.Lrc (LrcContext (..), mkLrcSyncData)
import           Pos.DB.Pure (DBPureVar, newDBPureVar)
import           Pos.DB.Ssc (mkSscState)
import           Pos.DB.Txp (GenericTxpLocalData, MempoolExt,
                     MonadTxpLocal (..), TxpGlobalSettings, TxpHolderTag,
                     mkTxpLocalData, txNormalize, txProcessTransactionNoLock,
                     txpGlobalSettings)
import           Pos.DB.Update (UpdateContext, mkUpdateContext)
import           Pos.Generator.Block (BlockGenMode)
import           Pos.Generator.BlockEvent (SnapshotId)
import qualified Pos.GState as GS
import           Pos.Infra.Network.Types (HasNodeType (..), NodeType (..))
import           Pos.Infra.Slotting (HasSlottingVar (..), MonadSimpleSlotting,
                     MonadSlots (..), SimpleSlottingMode,
                     SimpleSlottingStateVar, currentTimeSlottingSimple,
                     getCurrentSlotBlockingSimple,
                     getCurrentSlotBlockingSimple',
                     getCurrentSlotInaccurateSimple,
                     getCurrentSlotInaccurateSimple', getCurrentSlotSimple,
                     getCurrentSlotSimple', mkSimpleSlottingStateVar)
import           Pos.Infra.Slotting.Types (SlottingData)
import           Pos.Launcher.Configuration (HasConfigurations)
import           Pos.Util (newInitFuture, postfixLFields, postfixLFields2)
import           Pos.Util.CompileInfo (withCompileInfo)
import           Pos.Util.LoggerName (HasLoggerName' (..), askLoggerNameDefault,
                     modifyLoggerNameDefault)
import           Pos.Util.Util (HasLens (..))
import           Pos.Util.Wlog (HasLoggerName (..), LoggerName)
import           Pos.WorkMode (EmptyMempoolExt)

import           Test.Pos.Block.Logic.Emulation (Emulation (..), runEmulation,
                     sudoLiftIO)
import           Test.Pos.Configuration (defaultTestBlockVersionData,
                     defaultTestGenesisSpec)
import           Test.Pos.Core.Arbitrary ()
import           Test.Pos.Core.Dummy (dummyEpochSlots)

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
    , _tpTxpConfiguration   :: !TxpConfiguration
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
        let _tpTxpConfiguration = TxpConfiguration 200 Set.empty
        _tpGenesisInitializer <- genGenesisInitializer
        return TestParams {..}

genGenesisInitializer :: Gen GenesisInitializer
genGenesisInitializer = do
    giTestBalance <- arbitrary
    giFakeAvvmBalance <- arbitrary
    giAvvmBalanceFactor <- arbitrary
    giUseHeavyDlg <- arbitrary
    giSeed <- arbitrary
    return GenesisInitializer {..}

-- This function creates 'CoreConfiguration' from 'TestParams' and
-- uses it to satisfy 'HasConfiguration'.
withTestParams :: TestParams -> (Core.Config -> r) -> r
withTestParams TestParams {..} f = f $ mkConfig _tpStartTime genesisSpec
  where
    genesisSpec = defaultTestGenesisSpec
        { gsInitializer      = _tpGenesisInitializer
        , gsBlockVersionData = _tpBlockVersionData
        }

----------------------------------------------------------------------------
-- Init mode with instances
----------------------------------------------------------------------------

-- The fields are lazy on purpose: this allows using them with
-- futures.
data TestInitModeContext = TestInitModeContext
    { timcDBPureVar        :: DBPureVar
    , timcSlottingVar      :: TVar SlottingData
    , timcSlottingStateVar :: SimpleSlottingStateVar
    , timcSystemStart      :: !Timestamp
    , timcLrcContext       :: LrcContext
    }

makeLensesWith postfixLFields ''TestInitModeContext

type TestInitMode = ReaderT TestInitModeContext IO

runTestInitMode :: TestInitModeContext -> TestInitMode a -> IO a
runTestInitMode ctx = flip runReaderT ctx

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
    , btcSSlottingStateVar :: !SimpleSlottingStateVar
    , btcUpdateContext     :: !UpdateContext
    , btcSscState          :: !SscState
    , btcTxpMem            :: !(GenericTxpLocalData EmptyMempoolExt)
    , btcTxpGlobalSettings :: !TxpGlobalSettings
    , btcSlotId            :: !(Maybe SlotId)
    -- ^ If this value is 'Just' we will return it as the current
    -- slot. Otherwise simple slotting is used.
    , btcParams            :: !TestParams
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

initBlockTestContext
    :: HasDlgConfiguration
    => Core.Config
    -> TestParams
    -> (BlockTestContext -> Emulation a)
    -> Emulation a
initBlockTestContext coreConfig tp@TestParams {..} callback = do
    clockVar <- Emulation ask
    dbPureVar <- newDBPureVar
    (futureLrcCtx, putLrcCtx) <- newInitFuture "lrcCtx"
    (futureSlottingVar, putSlottingVar) <- newInitFuture "slottingVar"
    systemStart <- Timestamp <$> currentTime
    let epochSlots = configEpochSlots coreConfig
    slottingState <- mkSimpleSlottingStateVar epochSlots
    genesisSecretKeys <- gsSecretKeys <$> configGeneratedSecretsThrow coreConfig
    let initCtx =
            TestInitModeContext
                dbPureVar
                futureSlottingVar
                slottingState
                systemStart
                futureLrcCtx
        initBlockTestContextDo = do
            initNodeDBs coreConfig
            _gscSlottingVar <- newTVarIO =<< GS.getSlottingData
            putSlottingVar _gscSlottingVar
            let btcLoggerName = "testing"
            lcLrcSync <- mkLrcSyncData >>= newTVarIO
            let _gscLrcContext = LrcContext {..}
            putLrcCtx _gscLrcContext
            btcUpdateContext <- mkUpdateContext epochSlots
            btcSscState <- mkSscState epochSlots
            _gscSlogGState <- mkSlogGState
            btcTxpMem <- mkTxpLocalData
            let btcTxpGlobalSettings = txpGlobalSettings coreConfig _tpTxpConfiguration
            let btcSlotId = Nothing
            let btcParams = tp
            let btcGState = GS.GStateContext {_gscDB = DB.PureDB dbPureVar, ..}
            btcDelegation <- mkDelegationVar
            btcPureDBSnapshots <- PureDBSnapshotsVar <$> newIORef Map.empty
            let btcAllSecrets = mkAllSecretsSimple genesisSecretKeys
            let btCtx = BlockTestContext {btcSystemStart = systemStart, btcSSlottingStateVar = slottingState, ..}
            liftIO $ flip runReaderT clockVar $ unEmulation $ callback btCtx
    sudoLiftIO $ runTestInitMode initCtx $ initBlockTestContextDo

----------------------------------------------------------------------------
-- ExecMode
----------------------------------------------------------------------------

data BlockTestContextTag

instance HasLens BlockTestContextTag BlockTestContext BlockTestContext where
    lensOf = identity

type BlockTestMode = ReaderT BlockTestContext Emulation

runBlockTestMode
    :: HasDlgConfiguration
    => Core.Config
    -> TestParams
    -> BlockTestMode a
    -> IO a
runBlockTestMode coreConfig tp action =
    runEmulation (getTimestamp $ tp ^. tpStartTime)
        $ initBlockTestContext coreConfig tp (runReaderT action)

----------------------------------------------------------------------------
-- Property
----------------------------------------------------------------------------

type BlockProperty = PropertyM BlockTestMode

-- | Convert 'BlockProperty' to 'Property' using given generator of
-- 'TestParams'.
blockPropertyToProperty
    :: (HasDlgConfiguration, Testable a)
    => Gen TestParams
    -> (Core.Config -> BlockProperty a)
    -> Property
blockPropertyToProperty tpGen blockProperty =
    forAll tpGen $ \tp -> withTestParams tp $ \coreConfig -> monadic
        (ioProperty . runBlockTestMode coreConfig tp)
        (blockProperty coreConfig)

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
       (HasDlgConfiguration, Testable a)
    => (Core.Config -> BlockProperty a)
    -> Property
blockPropertyTestable = blockPropertyToProperty arbitrary

----------------------------------------------------------------------------
-- Boilerplate TestInitContext instances
----------------------------------------------------------------------------

instance HasLens DBPureVar TestInitModeContext DBPureVar where
    lensOf = timcDBPureVar_L

instance HasLens LrcContext TestInitModeContext LrcContext where
    lensOf = timcLrcContext_L

instance HasLens SimpleSlottingStateVar TestInitModeContext SimpleSlottingStateVar where
    lensOf = timcSlottingStateVar_L

instance HasSlottingVar TestInitModeContext where
    slottingTimestamp = timcSystemStart_L
    slottingVar = timcSlottingVar_L

instance MonadDBRead TestInitMode where
    dbGet = DB.dbGetPureDefault
    dbIterSource = DB.dbIterSourcePureDefault
    dbGetSerBlock = const DB.dbGetSerBlockPureDefault
    dbGetSerUndo = const DB.dbGetSerUndoPureDefault
    dbGetSerBlund = const DB.dbGetSerBlundPureDefault

instance MonadDB TestInitMode where
    dbPut = DB.dbPutPureDefault
    dbWriteBatch = DB.dbWriteBatchPureDefault
    dbDelete = DB.dbDeletePureDefault
    dbPutSerBlunds = DB.dbPutSerBlundsPureDefault

instance MonadSlotsData ctx TestInitMode => MonadSlots ctx TestInitMode where
    getCurrentSlot           = getCurrentSlotSimple
    getCurrentSlotBlocking   = getCurrentSlotBlockingSimple
    getCurrentSlotInaccurate = getCurrentSlotInaccurateSimple
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

instance HasLens SimpleSlottingStateVar BlockTestContext SimpleSlottingStateVar where
      lensOf = btcSSlottingStateVarL

-- | Ignore reports.
-- FIXME it's a bad sign that we even need this instance.
-- The pieces of the software which the block generator uses should never
-- even try to report.
instance MonadReporting BlockTestMode where
    report _ = pure ()

-- | Ignore reports.
-- FIXME it's a bad sign that we even need this instance.
instance HasMisbehaviorMetrics BlockTestContext where
    misbehaviorMetrics = lens (const Nothing) const

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
    askLoggerName = askLoggerNameDefault
    modifyLoggerName = modifyLoggerNameDefault

type TestSlottingContext ctx m =
    ( MonadSimpleSlotting ctx m
    , HasLens BlockTestContextTag ctx BlockTestContext
    )

testSlottingHelper
    :: TestSlottingContext ctx m
    => (SimpleSlottingStateVar -> m a)
    -> (SlotId -> a)
    -> m a
testSlottingHelper targetF alternative = do
    BlockTestContext{..} <- view (lensOf @BlockTestContextTag)
    case btcSlotId of
        Nothing   -> targetF btcSSlottingStateVar
        Just slot -> pure $ alternative slot

getCurrentSlotTestDefault :: TestSlottingContext ctx m => m (Maybe SlotId)
getCurrentSlotTestDefault =
    testSlottingHelper (getCurrentSlotSimple' dummyEpochSlots) Just

getCurrentSlotBlockingTestDefault :: TestSlottingContext ctx m => m SlotId
getCurrentSlotBlockingTestDefault =
    testSlottingHelper (getCurrentSlotBlockingSimple' dummyEpochSlots) identity

getCurrentSlotInaccurateTestDefault :: TestSlottingContext ctx m => m SlotId
getCurrentSlotInaccurateTestDefault = testSlottingHelper
    (getCurrentSlotInaccurateSimple' dummyEpochSlots)
    identity

currentTimeSlottingTestDefault :: SimpleSlottingMode ctx m => m Timestamp
currentTimeSlottingTestDefault = currentTimeSlottingSimple

instance MonadSlotsData ctx BlockTestMode => MonadSlots ctx BlockTestMode where
    getCurrentSlot = const getCurrentSlotTestDefault
    getCurrentSlotBlocking = const getCurrentSlotBlockingTestDefault
    getCurrentSlotInaccurate = const getCurrentSlotInaccurateTestDefault
    currentTimeSlotting = currentTimeSlottingTestDefault

instance MonadDBRead BlockTestMode where
    dbGet = DB.dbGetPureDefault
    dbIterSource = DB.dbIterSourcePureDefault
    dbGetSerBlock = const DB.dbGetSerBlockPureDefault
    dbGetSerUndo = const DB.dbGetSerUndoPureDefault
    dbGetSerBlund = const DB.dbGetSerBlundPureDefault

instance MonadDB BlockTestMode where
    dbPut = DB.dbPutPureDefault
    dbWriteBatch = DB.dbWriteBatchPureDefault
    dbDelete = DB.dbDeletePureDefault
    dbPutSerBlunds = DB.dbPutSerBlundsPureDefault

instance MonadGState BlockTestMode where
    gsAdoptedBVData = gsAdoptedBVDataDefault

instance MonadBListener BlockTestMode where
    onApplyBlocks = onApplyBlocksStub
    onRollbackBlocks _ = onRollbackBlocksStub

type instance MempoolExt BlockTestMode = EmptyMempoolExt

instance HasConfigurations => MonadTxpLocal (BlockGenMode EmptyMempoolExt BlockTestMode) where
    txpNormalize = withCompileInfo $ txNormalize
    txpProcessTx = withCompileInfo $ txProcessTransactionNoLock

instance MonadTxpLocal BlockTestMode where
    txpNormalize = withCompileInfo $ txNormalize
    txpProcessTx = withCompileInfo $ txProcessTransactionNoLock
