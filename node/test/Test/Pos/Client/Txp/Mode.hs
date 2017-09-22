{-# LANGUAGE TypeFamilies #-}

-- | Execution mode for tx creation tests.

module Test.Pos.Client.Txp.Mode
       ( TxpTestProperty
       , TxpTestMode
       , HasTxpConfigurations
       , ttcParams_L    -- to get rid of a warning
       ) where

import           Universum

import           Control.Lens                     (lens, makeLensesWith)
import qualified Data.ByteString                  as BS
import           Ether.Internal                   (HasLens (..))
import           Mockable                         (Production, currentTime, runProduction)
import           Test.QuickCheck                  (Arbitrary (..), Gen, Property,
                                                   Testable (..), forAll, ioProperty)
import           Test.QuickCheck.Monadic          (PropertyM, monadic)

import           Pos.Block.Core                   (Block, BlockHeader)
import           Pos.Block.Slog                   (mkSlogGState)
import           Pos.Block.Types                  (Undo)
import           Pos.Client.Txp.Addresses         (MonadAddresses (..))
import           Pos.Client.Txp.Util              (TxCreateMode)
import           Pos.Configuration                (HasNodeConfiguration)
import           Pos.Core                         (HasConfiguration, IsHeader, SlotId,
                                                   Timestamp (..))
import           Pos.DB                           (MonadGState (..))
import qualified Pos.DB                           as DB
import qualified Pos.DB.Block                     as DB
import           Pos.DB.DB                        as DB
import           Pos.DB.DB                        (initNodeDBs)
import qualified Pos.GState                       as GS
import           Pos.Infra.Configuration          (HasInfraConfiguration)
import           Pos.Lrc                          (LrcContext (..), mkLrcSyncData)
import           Pos.Slotting                     (HasSlottingVar (..), MonadSlots (..),
                                                   MonadSlotsData, SimpleSlottingVar,
                                                   mkSimpleSlottingVar)
import qualified Pos.Slotting                     as Slot
import           Pos.Ssc.Class                    (SscBlock)
import           Pos.Ssc.Class.Helpers            (SscHelpersClass)
import           Pos.Ssc.GodTossing               (SscGodTossing)
import           Pos.Ssc.GodTossing.Configuration (HasGtConfiguration)
import           Pos.Update.Configuration         (HasUpdateConfiguration)
import           Pos.Util.Util                    (Some, postfixLFields)

-- Need Emulation because it has instance Mockable CurrentTime
import           Test.Pos.Block.Logic.Emulation   (Emulation (..), runEmulation)
import           Test.Pos.Block.Logic.Mode        (TestParams (..))
import           Test.Pos.Client.Txp.Util         (generateAddressWithKey, seedSize)

----------------------------------------------------------------------------
-- TestParams
----------------------------------------------------------------------------

-- An object of this type has instance Arbitrary and is used as the source
-- of randomness in the tests.
type TxpTestParams = TestParams

type HasTxpConfigurations =
       ( HasNodeConfiguration
       , HasGtConfiguration
       , HasConfiguration
       , HasInfraConfiguration
       , HasUpdateConfiguration
       )

----------------------------------------------------------------------------
-- Mock for TxCreateMode
----------------------------------------------------------------------------

type TxpTestMode = ReaderT TxpTestContext Emulation

data TxpTestContext = TxpTestContext
    { ttcGState       :: !GS.GStateContext
    , ttcSystemStart  :: !Timestamp
    , ttcSSlottingVar :: !SimpleSlottingVar
    , ttcSlotId       :: !(Maybe SlotId)
    -- ^ If this value is 'Just' we will return it as the current
    -- slot. Otherwise simple slotting is used.
    , ttcParams       :: !TxpTestParams
    }

makeLensesWith postfixLFields ''TxpTestContext

instance HasTxpConfigurations => TxCreateMode TxpTestMode

----------------------------------------------------------------------------
-- Mock initialization
----------------------------------------------------------------------------

data TxpTestInitContext = TxpTestInitContext
    { tticDBPureVar      :: !DB.DBPureVar
    -- , tticGenesisContext :: !GenesisContext
    }

makeLensesWith postfixLFields ''TxpTestInitContext

type TxpTestInitMode = ReaderT TxpTestInitContext Production

runTestInitMode :: TxpTestInitContext -> TxpTestInitMode a -> IO a
runTestInitMode ctx = runProduction . flip runReaderT ctx

initTxpTestContext
    :: (HasTxpConfigurations, MonadIO m)
    => TxpTestParams
    -> m TxpTestContext
initTxpTestContext tp@TestParams {..} = do
    dbPureVar <- DB.newDBPureVar
    let initCtx = TxpTestInitContext
            { tticDBPureVar      = dbPureVar
            -- , tticGenesisContext = _ttpGenesisContext
            }
    liftIO $ runTestInitMode initCtx $ do
        initNodeDBs @SscGodTossing
        lcLrcSync <- newTVarIO =<< mkLrcSyncData
        let _gscLrcContext = LrcContext {..}
        _gscSlogGState <- mkSlogGState
        _gscSlottingVar <- newTVarIO =<< GS.getSlottingData
        let ttcGState = GS.GStateContext {_gscDB = DB.PureDB dbPureVar, ..}
        ttcSSlottingVar <- mkSimpleSlottingVar
        ttcSystemStart <- Timestamp <$> currentTime
        let ttcSlotId = Nothing
            ttcParams = tp
        pure TxpTestContext {..}

runTxpTestMode
    :: HasTxpConfigurations
    => TxpTestParams
    -> TxpTestMode a
    -> IO a
runTxpTestMode tp action = do
    ctx <- initTxpTestContext tp
    runEmulation (_tpStartTime tp) $ runReaderT action ctx

----------------------------------------------------------------------------
-- Boilerplate TxpTestInitContext instances
----------------------------------------------------------------------------

instance HasLens DB.DBPureVar TxpTestInitContext DB.DBPureVar where
    lensOf = tticDBPureVar_L

{-
instance HasLens GenesisContext TxpTestInitContext GenesisContext where
    lensOf = tticGenesisContext_L

instance HasLens GenesisUtxo TxpTestInitContext GenesisUtxo where
    lensOf = tticGenesisContext_L . gtcUtxo

instance HasLens GenesisWStakeholders TxpTestInitContext GenesisWStakeholders where
    lensOf = tticGenesisContext_L . gtcWStakeholders
-}

----------------------------------------------------------------------------
-- Boilerplate TxpTestInitMode instances
----------------------------------------------------------------------------

instance HasTxpConfigurations => DB.MonadDBRead TxpTestInitMode where
    dbGet = DB.dbGetPureDefault
    dbIterSource = DB.dbIterSourcePureDefault

instance HasTxpConfigurations => DB.MonadDB TxpTestInitMode where
    dbPut = DB.dbPutPureDefault
    dbWriteBatch = DB.dbWriteBatchPureDefault
    dbDelete = DB.dbDeletePureDefault

instance
    (HasTxpConfigurations, SscHelpersClass ssc) =>
    DB.MonadBlockDBGeneric (BlockHeader ssc) (Block ssc) Undo TxpTestInitMode
  where
    dbGetBlock  = DB.dbGetBlockPureDefault @ssc
    dbGetUndo   = DB.dbGetUndoPureDefault @ssc
    dbGetHeader = DB.dbGetHeaderPureDefault @ssc

instance (HasTxpConfigurations, SscHelpersClass ssc) =>
         DB.MonadBlockDBGenericWrite (BlockHeader ssc) (Block ssc) Undo TxpTestInitMode where
    dbPutBlund = DB.dbPutBlundPureDefault

instance
    (HasTxpConfigurations, SscHelpersClass ssc) =>
    DB.MonadBlockDBGeneric (Some IsHeader) (SscBlock ssc) () TxpTestInitMode
  where
    dbGetBlock  = DB.dbGetBlockSscPureDefault @ssc
    dbGetUndo   = DB.dbGetUndoSscPureDefault @ssc
    dbGetHeader = DB.dbGetHeaderSscPureDefault @ssc

----------------------------------------------------------------------------
-- Boilerplate TxpTestContext instances
----------------------------------------------------------------------------

instance GS.HasGStateContext TxpTestContext where
    gStateContext = ttcGState_L

instance HasSlottingVar TxpTestContext where
    slottingTimestamp = ttcSystemStart_L
    slottingVar = GS.gStateContext . GS.gscSlottingVar

{-
instance HasLens GenesisWStakeholders TxpTestContext GenesisWStakeholders where
    lensOf = ttcParams_L . ttpGenesisContext . gtcWStakeholders
-}

instance HasLens DB.DBPureVar TxpTestContext DB.DBPureVar where
    lensOf = GS.gStateContext . GS.gscDB . pureDBLens
      where
        getter = \case
            DB.RealDB _   -> realDBInTestsError
            DB.PureDB pdb -> pdb
        setter _ pdb = DB.PureDB pdb
        pureDBLens = lens getter setter
        realDBInTestsError = error "You are using real db in tests"

----------------------------------------------------------------------------
-- Boilerplate TxpTestMode instances
----------------------------------------------------------------------------

instance HasTxpConfigurations => MonadGState TxpTestMode where
    gsAdoptedBVData = DB.gsAdoptedBVDataDefault

instance (HasTxpConfigurations, MonadSlotsData ctx TxpTestMode)
      => MonadSlots ctx TxpTestMode
  where
    getCurrentSlot = do
        view ttcSlotId_L >>= \case
            Nothing -> Slot.getCurrentSlotSimple =<< view ttcSSlottingVar_L
            Just slot -> pure (Just slot)
    getCurrentSlotBlocking =
        view ttcSlotId_L >>= \case
            Nothing -> Slot.getCurrentSlotBlockingSimple =<< view ttcSSlottingVar_L
            Just slot -> pure slot
    getCurrentSlotInaccurate = do
        view ttcSlotId_L >>= \case
            Nothing -> Slot.getCurrentSlotInaccurateSimple =<< view ttcSSlottingVar_L
            Just slot -> pure slot
    -- FIXME: this has already caused a bug with Explorer (CSE-203).
    -- The workaround was to replace it with a `Timestamp 0`.
    currentTimeSlotting = Slot.currentTimeSlottingSimple

instance HasTxpConfigurations => DB.MonadDBRead TxpTestMode where
    dbGet = DB.dbGetPureDefault
    dbIterSource = DB.dbIterSourcePureDefault

instance HasTxpConfigurations => DB.MonadDB TxpTestMode where
    dbPut = DB.dbPutPureDefault
    dbWriteBatch = DB.dbWriteBatchPureDefault
    dbDelete = DB.dbDeletePureDefault

instance HasTxpConfigurations => MonadAddresses TxpTestMode where
    type AddrData TxpTestMode = ()
    getNewAddress _ = pure address
      where
        -- seed for address generation is a ByteString with 32 255's
        seed = BS.replicate seedSize (255 :: Word8)
        (_, address) = generateAddressWithKey seed

----------------------------------------------------------------------------
-- Property
----------------------------------------------------------------------------

type TxpTestProperty = PropertyM TxpTestMode

-- Cannot write a general OVERLAPPABLE instance with MonadTrans since
-- type families cannot be OVERLAPPABLE.
instance HasTxpConfigurations => MonadAddresses TxpTestProperty where
    type AddrData TxpTestProperty = AddrData TxpTestMode
    getNewAddress = lift . getNewAddress

txCreatePropertyToProperty
    :: HasTxpConfigurations
    => Gen TxpTestParams
    -> TxpTestProperty a
    -> Property
txCreatePropertyToProperty tpGen txpTestProperty =
    forAll tpGen $ \tp ->
        monadic (ioProperty . runTxpTestMode tp) txpTestProperty

instance HasTxpConfigurations => Testable (TxpTestProperty a) where
    property = txCreatePropertyToProperty arbitrary
