{-# LANGUAGE TypeFamilies #-}

module Pos.Explorer.ExplorerMode
    ( -- Explorer
      ExplorerMode
    , ExplorerTestMode
    , ExplorerTestParams
    , runExplorerTestMode
    , etcParams_L
    , ExplorerProperty
    , explorerPropertyToProperty
    -- Explorer Socket Subscription
    , SubscriptionTestMode
    , runSubTestMode
    ) where

import           Universum

import           Control.Lens (lens, makeLensesWith)
import           System.Wlog (CanLog, HasLoggerName (..), LoggerName (..))

import           Test.QuickCheck (Gen, Property, Testable (..), arbitrary, forAll, ioProperty)
import           Test.QuickCheck.Monadic (PropertyM, monadic)

import           Pos.Block.Slog (mkSlogGState)
import           Pos.Core (SlotId, Timestamp (..))
import           Pos.DB (MonadGState (..))
import qualified Pos.DB as DB
import qualified Pos.DB.Block as DB
import           Pos.DB.Class (MonadDBRead)
import           Pos.DB.DB as DB
import qualified Pos.GState as GS
import           Pos.Lrc (LrcContext (..), mkLrcSyncData)
import           Pos.Slotting (HasSlottingVar (..), MonadSlots (..), MonadSlotsData,
                               SimpleSlottingVar, mkSimpleSlottingVar)
import qualified Pos.Slotting as Slot
import           Pos.Txp (GenericTxpLocalData (..), MempoolExt, MonadTxpMem, TxpHolderTag,
                          mkTxpLocalData)
import           Pos.Util (postfixLFields)
import           Pos.Util.Util (HasLens (..))

import           Pos.Explorer.ExtraContext (ExtraContext, ExtraContextT, HasExplorerCSLInterface,
                                            HasGenesisRedeemAddressInfo, makeExtraCtx,
                                            runExtraContextT)
import           Pos.Explorer.Socket.Holder (ConnectionsState)
import           Pos.Explorer.Txp (ExplorerExtra (..))

-- Need Emulation because it has instance Mockable CurrentTime
import           Mockable (Production, currentTime, runProduction)
import           Pos.Launcher.Configuration (HasConfigurations)
import           Pos.Util.JsonLog (HasJsonLogConfig (..), jsonLogDefault)
import           Pos.Util.LoggerName (HasLoggerName' (..), askLoggerNameDefault,
                                      modifyLoggerNameDefault)
import           Pos.Util.TimeWarp (CanJsonLog (..))
import           Pos.WorkMode (MinWorkMode)
import           Test.Pos.Block.Logic.Emulation (Emulation (..), runEmulation)
import           Test.Pos.Block.Logic.Mode (TestParams (..))


-------------------------------------------------------------------------------------
-- Explorer mode
-------------------------------------------------------------------------------------

-- | We require much less then @WorkMode@, and this simplifies things later when
-- testing (and running).
type ExplorerMode ctx m =
    ( MonadDBRead m
    -- Database operations
    , MonadSlots ctx m
    -- Slotting
    , MonadThrow m
    , MonadCatch m
    , MonadMask m
    -- General utility operations
    , HasExplorerCSLInterface m
    -- For mocking external functions
    , HasGenesisRedeemAddressInfo m
    -- Genesis operations
    , MonadTxpMem (MempoolExt m) ctx m
    -- Txp, could be @TxpLocalWorkMode@
    , MinWorkMode m
    -- The rest of the constraints - logger, mockable, configurations
    )

----------------------------------------------------------------------------
-- TestParams
----------------------------------------------------------------------------

-- An object of this type has instance Arbitrary and is used as the source
-- of randomness in the tests.
type ExplorerTestParams = TestParams

----------------------------------------------------------------------------
-- Mock for ExplorerTestMode
----------------------------------------------------------------------------

-- | Test mode to run with. Using @Emulation@.
type ExplorerTestMode = ReaderT ExplorerTestContext Emulation

-- | Test mode with extra context so we can mock out the external functions.
type ExplorerExtraTestMode = ExtraContextT ExplorerTestMode

data ExplorerTestContext = ExplorerTestContext
    { etcGState       :: !GS.GStateContext
    , etcSystemStart  :: !Timestamp
    , etcSSlottingVar :: !SimpleSlottingVar
    , etcSlotId       :: !(Maybe SlotId)
    -- ^ If this value is 'Just' we will return it as the current
    -- slot. Otherwise simple slotting is used.
    , etcTxpLocalData :: !(GenericTxpLocalData ExplorerExtra)
    , etcLoggerName   :: !LoggerName
    , etcParams       :: !ExplorerTestParams
    }

makeLensesWith postfixLFields ''ExplorerTestContext

----------------------------------------------------------------------------
-- Mock initialization
----------------------------------------------------------------------------

data ExplorerTestInitContext = ExplorerTestInitContext
    { eticDBPureVar      :: !DB.DBPureVar
    }

makeLensesWith postfixLFields ''ExplorerTestInitContext

type ExplorerTestInitMode = ReaderT ExplorerTestInitContext Production

runTestInitMode :: ExplorerTestInitContext -> ExplorerTestInitMode a -> IO a
runTestInitMode ctx = runProduction . usingReaderT ctx

initExplorerTestContext
    :: (HasConfigurations, MonadIO m)
    => ExplorerTestParams
    -> m ExplorerTestContext
initExplorerTestContext tp@TestParams {..} = do
    dbPureVar <- DB.newDBPureVar
    let initCtx = ExplorerTestInitContext
            { eticDBPureVar      = dbPureVar
            }
    liftIO $ runTestInitMode initCtx $ do
        DB.initNodeDBs
        lcLrcSync <- newTVarIO =<< mkLrcSyncData
        let _gscLrcContext = LrcContext {..}
        _gscSlogGState <- mkSlogGState
        _gscSlottingVar <- newTVarIO =<< GS.getSlottingData
        let etcGState = GS.GStateContext {_gscDB = DB.PureDB dbPureVar, ..}
        etcSSlottingVar <- mkSimpleSlottingVar
        etcSystemStart <- Timestamp <$> currentTime
        etcTxpLocalData <- mkTxpLocalData

        let etcSlotId       = Nothing
            etcParams       = tp
            etcLoggerName   = "explorertesting"
        pure ExplorerTestContext {..}

-- | Run test mode with @ExtraContext@ so we can mock the functions.
runExplorerTestMode
    :: HasConfigurations
    => ExplorerTestParams
    -> ExtraContext
    -> ExplorerExtraTestMode a
    -> IO a
runExplorerTestMode tp extraContext action = do
    ctx <- initExplorerTestContext tp
    runEmulation (getTimestamp $ _tpStartTime tp) $
        usingReaderT ctx (runExtraContextT extraContext action)

----------------------------------------------------------------------------
-- Boilerplate ExplorerTestInitContext instances
----------------------------------------------------------------------------

instance HasLens DB.DBPureVar ExplorerTestInitContext DB.DBPureVar where
    lensOf = eticDBPureVar_L

----------------------------------------------------------------------------
-- Boilerplate ExplorerTestInitMode instances
----------------------------------------------------------------------------

instance HasConfigurations => DB.MonadDBRead ExplorerTestInitMode where
    dbGet = DB.dbGetPureDefault
    dbIterSource = DB.dbIterSourcePureDefault
    dbGetSerBlock = DB.dbGetSerBlockPureDefault
    dbGetSerUndo = DB.dbGetSerUndoPureDefault

instance HasConfigurations => DB.MonadDB ExplorerTestInitMode where
    dbPut = DB.dbPutPureDefault
    dbWriteBatch = DB.dbWriteBatchPureDefault
    dbDelete = DB.dbDeletePureDefault
    dbPutSerBlunds = DB.dbPutSerBlundsPureDefault

----------------------------------------------------------------------------
-- Boilerplate ExplorerTestContext instances
----------------------------------------------------------------------------

instance GS.HasGStateContext ExplorerTestContext where
    gStateContext = etcGState_L

instance HasSlottingVar ExplorerTestContext where
    slottingTimestamp = etcSystemStart_L
    slottingVar = GS.gStateContext . GS.gscSlottingVar

instance HasLens DB.DBPureVar ExplorerTestContext DB.DBPureVar where
    lensOf = GS.gStateContext . GS.gscDB . pureDBLens
      where
        getter = \case
            DB.RealDB _   -> realDBInTestsError
            DB.PureDB pdb -> pdb
        setter _ pdb = DB.PureDB pdb
        pureDBLens = lens getter setter
        realDBInTestsError = error "You are using real db in tests"

-- We need to define the full transformer stack type.
type instance MempoolExt ExplorerExtraTestMode = ExplorerExtra

instance HasLens TxpHolderTag ExplorerTestContext (GenericTxpLocalData ExplorerExtra) where
    lensOf = etcTxpLocalData_L

instance HasLens LoggerName ExplorerTestContext LoggerName where
      lensOf = etcLoggerName_L

instance HasLoggerName' ExplorerTestContext where
    loggerName = lensOf @LoggerName

instance HasJsonLogConfig ExplorerTestContext where
    jsonLogConfig = jsonLogConfig

----------------------------------------------------------------------------
-- Boilerplate ExplorerTestMode instances
----------------------------------------------------------------------------

instance HasConfigurations => MonadGState ExplorerTestMode where
    gsAdoptedBVData = DB.gsAdoptedBVDataDefault

instance (HasConfigurations, MonadSlotsData ctx ExplorerTestMode)
      => MonadSlots ctx ExplorerTestMode
  where
    getCurrentSlot = do
        view etcSlotId_L >>= \case
            Nothing -> Slot.getCurrentSlotSimple =<< view etcSSlottingVar_L
            Just slot -> pure (Just slot)
    getCurrentSlotBlocking =
        view etcSlotId_L >>= \case
            Nothing -> Slot.getCurrentSlotBlockingSimple =<< view etcSSlottingVar_L
            Just slot -> pure slot
    getCurrentSlotInaccurate = do
        view etcSlotId_L >>= \case
            Nothing -> Slot.getCurrentSlotInaccurateSimple =<< view etcSSlottingVar_L
            Just slot -> pure slot
    currentTimeSlotting = Slot.currentTimeSlottingSimple

instance HasConfigurations => DB.MonadDBRead ExplorerTestMode where
    dbGet = DB.dbGetPureDefault
    dbIterSource = DB.dbIterSourcePureDefault
    dbGetSerBlock = DB.dbGetSerBlockPureDefault
    dbGetSerUndo = DB.dbGetSerUndoPureDefault

instance HasConfigurations => DB.MonadDB ExplorerTestMode where
    dbPut = DB.dbPutPureDefault
    dbWriteBatch = DB.dbWriteBatchPureDefault
    dbDelete = DB.dbDeletePureDefault
    dbPutSerBlunds = DB.dbPutSerBlundsPureDefault

instance {-# OVERLAPPING #-} HasLoggerName ExplorerTestMode where
    askLoggerName = askLoggerNameDefault
    modifyLoggerName = modifyLoggerNameDefault

instance {-# OVERLAPPING #-} CanJsonLog ExplorerTestMode where
    jsonLog = jsonLogDefault


----------------------------------------------------------------------------
-- SubscriptionMode
----------------------------------------------------------------------------

newtype SubscriptionTestMode a = SubscriptionTestMode
    { runSubscriptionTestMode :: (StateT ConnectionsState IO a)
    } deriving (Functor, Applicative, Monad, MonadThrow, CanLog, MonadState ConnectionsState)

runSubTestMode :: ConnectionsState -> SubscriptionTestMode a -> IO (a, ConnectionsState)
runSubTestMode connectionsState m =
    runStateT (runSubscriptionTestMode m) connectionsState

instance HasLoggerName SubscriptionTestMode where
    askLoggerName        = pure "explorer-subscription-test"
    modifyLoggerName _ a = a

----------------------------------------------------------------------------
-- Property
----------------------------------------------------------------------------

type ExplorerProperty = PropertyM ExplorerExtraTestMode

explorerPropertyToProperty
    :: (HasConfigurations, Testable a)
    => Gen ExplorerTestParams
    -> ExplorerProperty a
    -> Property
explorerPropertyToProperty tpGen explorerTestProperty =
    forAll tpGen $ \tp ->
        monadic (ioProperty . (runExplorerTestMode tp makeExtraCtx)) explorerTestProperty

instance (HasConfigurations, Testable a) => Testable (ExplorerProperty a) where
    property = explorerPropertyToProperty arbitrary
