{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

-- | Execution modes for block logic tests.

module Test.Pos.Block.Logic.Mode
       ( TestParams (..)
       , TestInitModeContext (..)
       , BlockTestContextTag
       , BlockTestContext(..)
       , BlockTestMode
       , runBlockTestMode

       , BlockProperty
       ) where

import           Universum

import           Control.Lens            (makeLensesWith)
import qualified Control.Monad.Reader    as Mtl
import qualified Data.HashMap.Strict     as HM
import qualified Data.Map.Strict         as M
import qualified Data.Text.Buildable
import           Ether.Internal          (HasLens (..))
import           Formatting              (bprint, build, formatToString, int, shown, (%))
import           Mockable                (Production, currentTime, runProduction)
import qualified Prelude
import           Serokell.Util           (listJson)
import           System.Wlog             (HasLoggerName (..), LoggerName)
import           Test.QuickCheck         (Arbitrary (..), Gen, Testable (..), choose,
                                          ioProperty, oneof)
import           Test.QuickCheck.Monadic (PropertyM, monadic)

import           Pos.Block.Core          (Block, BlockHeader)
import           Pos.Block.Types         (Undo)
import           Pos.Context             (GenesisUtxo (..))
import           Pos.Core                (IsHeader, StakeDistribution (..), StakeholderId,
                                          Timestamp (..), addressHash, makePubKeyAddress,
                                          mkCoin, unsafeGetCoin)
import           Pos.Crypto              (SecretKey, toPublic, unsafeHash)
import           Pos.DB                  (MonadBlockDBGeneric (..),
                                          MonadBlockDBGenericWrite (..), MonadDB (..),
                                          MonadDBRead (..), MonadGState (..))
import qualified Pos.DB                  as DB
import qualified Pos.DB.Block            as DB
import           Pos.DB.DB               (gsAdoptedBVDataDefault, initNodeDBs)
import qualified Pos.DB.GState           as GState
import           Pos.DB.Pure             (DBPureVar, newDBPureVar)
import           Pos.Genesis             (stakeDistribution)
import           Pos.Launcher            (newInitFuture)
import           Pos.Lrc                 (LrcContext (..), mkLrcSyncData)
import           Pos.Slotting            (HasSlottingVar (..), MonadSlots (..),
                                          SlottingData, currentTimeSlottingSimple,
                                          getCurrentSlotBlockingSimple,
                                          getCurrentSlotInaccurateSimple,
                                          getCurrentSlotSimple)
import           Pos.Slotting.MemState   (MonadSlotsData (..), getSlottingDataDefault,
                                          getSystemStartDefault, putSlottingDataDefault,
                                          waitPenultEpochEqualsDefault)
import           Pos.Ssc.Class           (SscBlock)
import           Pos.Ssc.Class.Helpers   (SscHelpersClass)
import           Pos.Ssc.Extra           (SscMemTag, SscState, mkSscState)
import           Pos.Ssc.GodTossing      (SscGodTossing)
import           Pos.Txp                 (TxIn (..), TxOut (..), TxOutAux (..),
                                          TxpGlobalSettings, txpGlobalSettings, utxoF)
import           Pos.Update.Context      (UpdateContext, mkUpdateContext)
import           Pos.Util.LoggerName     (HasLoggerName' (..), getLoggerNameDefault,
                                          modifyLoggerNameDefault)
import           Pos.Util.Util           (Some, postfixLFields)

-- TODO: it shouldn't be 'Production', but currently we don't have anything else.
-- Expect some changes here somewhere in 2019.
type BaseMonad = Production

----------------------------------------------------------------------------
-- Parameters
----------------------------------------------------------------------------

-- | This data type contains all parameters which should be generated
-- before testing starts.
data TestParams = TestParams
    { tpGenUtxo           :: !GenesisUtxo
    -- ^ Genesis 'Utxo'.
    , tpSecretKeys        :: !(HashMap StakeholderId SecretKey)
    -- ^ Secret keys corresponding to 'PubKeyAddress'es from
    -- genesis 'Utxo'.
    -- They are stored in map (with 'StakeholderId' as key) to make it easy
    -- to find 'SecretKey' corresponding to given 'StakeholderId'.
    -- In tests we often want to have inverse of 'hash' and 'toPublic'.
    , tpStakeDistribution :: !StakeDistribution
    -- ^ Stake distribution which was used to generate genesis utxo.
    -- It's primarily needed to see which distribution was used (e. g.
    -- when test fails).
    }

instance Buildable TestParams where
    build TestParams {..} =
        bprint ("TestParams {\n"%
                "  utxo = "%utxoF%"\n"%
                "  secret keys: "%int%" items\n"%
                "  stake distribution: "%shown%"\n"%
                "  stakeholders: "%listJson%"\n"%
                "}\n")
            utxo
            (length tpSecretKeys)
            tpStakeDistribution
            (HM.keys tpSecretKeys)
      where
        utxo = tpGenUtxo & \(GenesisUtxo u) -> u

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
        let toSecretPair sk = (addressHash (toPublic sk), sk)
        let tpSecretKeys = HM.fromList $ map toSecretPair secretKeysList
        tpStakeDistribution <-
            genSuitableStakeDistribution (fromIntegral $ length tpSecretKeys)
        let zipF secretKey (coin, toaDistr) =
                let addr = makePubKeyAddress (toPublic secretKey)
                    toaOut = TxOut addr coin
                in (TxIn (unsafeHash addr) 0, TxOutAux {..})
        let tpGenUtxo =
                GenesisUtxo . M.fromList $
                zipWith
                    zipF
                    (toList tpSecretKeys)
                    (stakeDistribution tpStakeDistribution)
        return TestParams {..}

----------------------------------------------------------------------------
-- Init mode with instances
----------------------------------------------------------------------------

-- The fields are lazy on purpose: this allows using them with
-- futures.
data TestInitModeContext ssc = TestInitModeContext
    { timcDBPureVar   :: DBPureVar
    , timcGenesisUtxo :: GenesisUtxo
    , timcSlottingVar :: (Timestamp, TVar SlottingData)
    , timcLrcContext  :: LrcContext
    }

makeLensesWith postfixLFields ''TestInitModeContext

type TestInitMode ssc = Mtl.ReaderT (TestInitModeContext ssc) Production

runTestInitMode :: TestInitModeContext ssc -> TestInitMode ssc a -> Production a
runTestInitMode = flip Mtl.runReaderT

----------------------------------------------------------------------------
-- Main context
----------------------------------------------------------------------------

data BlockTestContext = BlockTestContext
    { btcDBPureVar         :: !DBPureVar
    , btcSlottingVar       :: !(Timestamp, TVar SlottingData)
    , btcLoggerName        :: !LoggerName
    , btcLrcContext        :: !LrcContext
    , btcUpdateContext     :: !UpdateContext
    , btcSscState          :: !(SscState SscGodTossing)
    , btcTxpGlobalSettings :: !TxpGlobalSettings
    , btcParams            :: !TestParams
    }

makeLensesWith postfixLFields ''BlockTestContext

----------------------------------------------------------------------------
-- Initialization
----------------------------------------------------------------------------

initBlockTestContext ::
       TestParams -> (BlockTestContext -> BaseMonad a) -> BaseMonad a
initBlockTestContext testParams@TestParams {..} callback = do
    dbPureVar <- newDBPureVar
    (futureLrcCtx, putLrcCtx) <- newInitFuture
    (futureSlottingVar, putSlottingVar) <- newInitFuture
    let initCtx =
            TestInitModeContext
                dbPureVar
                tpGenUtxo
                futureSlottingVar
                futureLrcCtx
    runTestInitMode @SscGodTossing initCtx $
        initBlockTestContextDo dbPureVar putSlottingVar putLrcCtx
  where
    initBlockTestContextDo btcDBPureVar putSlottingVar putLrcCtx = do
        systemStart <- Timestamp <$> currentTime
        initNodeDBs @SscGodTossing systemStart
        slottingData <- GState.getSlottingData
        btcSlottingVar <- (systemStart, ) <$> newTVarIO slottingData
        putSlottingVar btcSlottingVar
        let btcLoggerName = "testing"
        lcLrcSync <- mkLrcSyncData >>= newTVarIO
        let btcLrcContext = LrcContext {..}
        putLrcCtx btcLrcContext
        btcUpdateContext <- mkUpdateContext
        btcSscState <- mkSscState @SscGodTossing
        let btcTxpGlobalSettings = txpGlobalSettings
        let btcParams = testParams
        lift $ callback BlockTestContext {..}

----------------------------------------------------------------------------
-- ExecMode
----------------------------------------------------------------------------

data BlockTestContextTag

instance HasLens BlockTestContextTag BlockTestContext BlockTestContext where
    lensOf = identity

type BlockTestMode = Mtl.ReaderT BlockTestContext BaseMonad

runBlockTestMode :: TestParams -> BlockTestMode a -> IO a
runBlockTestMode tp action =
    runProduction $ initBlockTestContext tp (runReaderT action)

----------------------------------------------------------------------------
-- Property
----------------------------------------------------------------------------

type BlockProperty = PropertyM BlockTestMode

instance Testable (BlockProperty a) where
    property blockProperty =
        property $ \testParams ->
            (monadic (ioProperty . runBlockTestMode testParams) blockProperty)

----------------------------------------------------------------------------
-- Boilerplate TestInitContext instances
----------------------------------------------------------------------------

instance HasLens DBPureVar (TestInitModeContext ssc) DBPureVar where
    lensOf = timcDBPureVar_L

instance HasLens GenesisUtxo (TestInitModeContext ssc) GenesisUtxo where
    lensOf = timcGenesisUtxo_L

--instance HasLens SlottingContextSum (TestInitModeContext ssc) SlottingContextSum where
--    lensOf = imcSlottingContextSum_L

instance HasLens LrcContext (TestInitModeContext ssc) LrcContext where
    lensOf = timcLrcContext_L

instance HasSlottingVar (TestInitModeContext ssc) where
    slottingTimestamp = timcSlottingVar_L . _1
    slottingVar = timcSlottingVar_L . _2

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
    getCurrentSlot = getCurrentSlotSimple
    getCurrentSlotBlocking = getCurrentSlotBlockingSimple
    getCurrentSlotInaccurate = getCurrentSlotInaccurateSimple
    currentTimeSlotting = currentTimeSlottingSimple

----------------------------------------------------------------------------
-- Boilerplate BlockTestContext instances
----------------------------------------------------------------------------

instance HasLens DBPureVar BlockTestContext DBPureVar where
      lensOf = btcDBPureVar_L

instance HasLens LoggerName BlockTestContext LoggerName where
      lensOf = btcLoggerName_L

instance HasLens LrcContext BlockTestContext LrcContext where
      lensOf = btcLrcContext_L

instance HasLens UpdateContext BlockTestContext UpdateContext where
      lensOf = btcUpdateContext_L

instance HasLens SscMemTag BlockTestContext (SscState SscGodTossing) where
      lensOf = btcSscState_L

instance HasLens TxpGlobalSettings BlockTestContext TxpGlobalSettings where
      lensOf = btcTxpGlobalSettings_L

instance HasLens TestParams BlockTestContext TestParams where
      lensOf = btcParams_L

instance HasSlottingVar BlockTestContext where
    slottingTimestamp = btcSlottingVar_L . _1
    slottingVar = btcSlottingVar_L . _2

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
    getCurrentSlot = getCurrentSlotSimple
    getCurrentSlotBlocking = getCurrentSlotBlockingSimple
    getCurrentSlotInaccurate = getCurrentSlotInaccurateSimple
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
    dbGetBlock  = DB.dbGetBlockPureDefault @SscGodTossing
    dbGetUndo   = DB.dbGetUndoPureDefault @SscGodTossing
    dbGetHeader = DB.dbGetHeaderPureDefault @SscGodTossing

instance MonadBlockDBGeneric (Some IsHeader) (SscBlock SscGodTossing) () BlockTestMode
  where
    dbGetBlock  = DB.dbGetBlockSscPureDefault @SscGodTossing
    dbGetUndo   = DB.dbGetUndoSscPureDefault @SscGodTossing
    dbGetHeader = DB.dbGetHeaderSscPureDefault @SscGodTossing

instance MonadBlockDBGenericWrite (BlockHeader SscGodTossing) (Block SscGodTossing) Undo BlockTestMode where
    dbPutBlund = DB.dbPutBlundPureDefault

instance MonadGState BlockTestMode where
    gsAdoptedBVData = gsAdoptedBVDataDefault
