{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

-- | Execution modes for block logic tests.

module Test.Pos.Block.Logic.Mode
       ( BlockTestContextTag
       , BlockTestContext(..)
       , BlockTestMode
       , unBlockTestMode
       , runBlockTestMode

       , BlockProperty
       ) where

import           Universum

import qualified Control.Monad.Reader    as Mtl
import qualified Data.Text.Buildable
import qualified Ether
import           Formatting              (bprint, build, formatToString, (%))
import           Mockable                (Production, currentTime, runProduction)
import qualified Prelude
import           System.IO.Temp          (withSystemTempDirectory)
import           System.Wlog             (HasLoggerName (..), LoggerName)
import           Test.QuickCheck         (Arbitrary (..), Testable (..), ioProperty)
import           Test.QuickCheck.Monadic (PropertyM, monadic)

import           Pos.Block.Core          (Block, BlockHeader)
import           Pos.Block.Types         (Undo)
import           Pos.Context             (GenesisUtxo (..))
import           Pos.Core                (IsHeader, Timestamp (..))
import           Pos.DB                  (MonadBlockDBGeneric (..), MonadDB (..),
                                          MonadDBRead (..), MonadGState (..), NodeDBs,
                                          dbDeleteDefault, dbGetDefault,
                                          dbIterSourceDefault, dbPutDefault,
                                          dbWriteBatchDefault)
import           Pos.DB.Block            (MonadBlockDBWrite (..), dbGetBlockDefault,
                                          dbGetBlockSscDefault, dbGetHeaderDefault,
                                          dbGetHeaderSscDefault, dbGetUndoDefault,
                                          dbGetUndoSscDefault, dbPutBlundDefault)
import           Pos.DB.DB               (closeNodeDBs, gsAdoptedBVDataDefault,
                                          initNodeDBs, openNodeDBs)
import qualified Pos.DB.GState           as GState
import           Pos.ExecMode            ((:::), ExecMode (..), ExecModeM, HasLens (..),
                                          modeContext)
import           Pos.Launcher            (InitModeContext (..), newInitFuture,
                                          runInitMode)
import           Pos.Lrc                 (LrcContext (..), mkLrcSyncData)
import           Pos.Slotting            (MonadSlots (..), SlottingContextSum (SCSimple),
                                          currentTimeSlottingSimple,
                                          getCurrentSlotBlockingSimple,
                                          getCurrentSlotInaccurateSimple,
                                          getCurrentSlotSimple)
import           Pos.Slotting.MemState   (MonadSlotsData (..), SlottingVar,
                                          getSlottingDataDefault, getSystemStartDefault,
                                          putSlottingDataDefault,
                                          waitPenultEpochEqualsDefault)
import           Pos.Ssc.Class           (SscBlock)
import           Pos.Ssc.Extra           (SscMemTag, SscState, mkSscState)
import           Pos.Ssc.GodTossing      (SscGodTossing)
import           Pos.Txp                 (TxpGlobalSettings, txpGlobalSettings, utxoF)
import           Pos.Update.Context      (UpdateContext, mkUpdateContext)
import           Pos.Util.Util           (Some)

-- TODO: it shouldn't be 'Production', but currently we don't have anything else.
-- Expect some changes here somewhere in 2019.
type BaseMonad = Production

----------------------------------------------------------------------------
-- Main context
----------------------------------------------------------------------------

modeContext [d|
    data BlockTestContext = BlockTestContext
        { btcDBs               :: !(NodeDBs     ::: NodeDBs)
        , btcSlottingVar       :: !(SlottingVar ::: SlottingVar)
        , btcLoggerName        :: !(LoggerName  ::: LoggerName)
        , btcLrcContext        :: !(LrcContext  ::: LrcContext)
        , btcUpdateContext     :: !(UpdateContext ::: UpdateContext)
        , btcSscState          :: !(SscMemTag     ::: SscState SscGodTossing)
        , btcTxpGlobalSettings :: !(TxpGlobalSettings ::: TxpGlobalSettings)
        }
    |]

----------------------------------------------------------------------------
-- Parameters
----------------------------------------------------------------------------

-- TODO
instance Arbitrary GenesisUtxo where
    arbitrary = pure $ GenesisUtxo mempty

-- | This data type contains all parameters which should be generated
-- before testing starts.
data TestParams = TestParams
    { tpGenUtxo :: !GenesisUtxo
    }

instance Buildable TestParams where
    build TestParams {..} =
        bprint ("TestParams {\n"%
                "  utxo = "%utxoF%"\n"%
                "}\n")
            utxo
      where
        utxo = case tpGenUtxo of GenesisUtxo u -> u

instance Show TestParams where
    show = formatToString build

instance Arbitrary TestParams where
    arbitrary = TestParams <$> arbitrary

----------------------------------------------------------------------------
-- Initialization
----------------------------------------------------------------------------

-- Maybe we will make everything pure somewhere in 2021, but for now
-- this is commented out and let's use 'bracket'.
-- initBlockTestContext :: BlockTestContext
-- initBlockTestContext = ¯\_(ツ)_/¯

-- So here we go. Bracket, yes.
bracketBlockTestContext ::
       TestParams -> (BlockTestContext -> BaseMonad a) -> BaseMonad a
bracketBlockTestContext TestParams {..} callback =
    withSystemTempDirectory "cardano-sl-testing" $ \dbPath ->
        bracket (openNodeDBs False dbPath) closeNodeDBs $ \nodeDBs -> do
            (futureLrcCtx, putLrcCtx) <- newInitFuture
            (futureSlottingVar, putSlottingVar) <- newInitFuture
            let initCtx =
                    InitModeContext
                        nodeDBs
                        tpGenUtxo
                        futureSlottingVar
                        SCSimple
                        futureLrcCtx
            runInitMode @SscGodTossing initCtx $
                bracketBlockTestContextDo nodeDBs putSlottingVar putLrcCtx
  where
    bracketBlockTestContextDo btcDBs putSlottingVar putLrcCtx = do
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
        ExecMode . lift $ callback BlockTestContext {..}

----------------------------------------------------------------------------
-- ExecMode
----------------------------------------------------------------------------

data BlockTestContextTag

instance HasLens BlockTestContextTag BlockTestContext BlockTestContext where
    lensOf = identity

data BTest

type BlockTestMode = ExecMode BTest

type instance ExecModeM BTest =
    Mtl.ReaderT BlockTestContext BaseMonad

unBlockTestMode :: ExecMode BTest a -> ExecModeM BTest a
unBlockTestMode = unExecMode

runBlockTestMode :: TestParams -> BlockTestMode a -> IO a
runBlockTestMode tp (unBlockTestMode -> action) =
    runProduction $ bracketBlockTestContext tp (runReaderT action)

----------------------------------------------------------------------------
-- Property
----------------------------------------------------------------------------

type BlockProperty = PropertyM BlockTestMode

instance Testable (BlockProperty a) where
    property blockProperty =
        property $ \testParams ->
            (monadic (ioProperty . runBlockTestMode testParams) blockProperty)

----------------------------------------------------------------------------
-- Boilerplate instances
----------------------------------------------------------------------------

-- Test mode

instance HasLoggerName BlockTestMode where
    getLoggerName = Ether.ask'
    modifyLoggerName = Ether.local'

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
    dbGet = dbGetDefault
    dbIterSource = dbIterSourceDefault

instance MonadDB BlockTestMode where
    dbPut = dbPutDefault
    dbWriteBatch = dbWriteBatchDefault
    dbDelete = dbDeleteDefault

instance MonadBlockDBWrite SscGodTossing BlockTestMode where
    dbPutBlund = dbPutBlundDefault

instance MonadBlockDBGeneric (BlockHeader SscGodTossing) (Block SscGodTossing) Undo BlockTestMode
  where
    dbGetBlock  = dbGetBlockDefault @SscGodTossing
    dbGetUndo   = dbGetUndoDefault @SscGodTossing
    dbGetHeader = dbGetHeaderDefault @SscGodTossing

instance MonadBlockDBGeneric (Some IsHeader) (SscBlock SscGodTossing) () BlockTestMode
  where
    dbGetBlock  = dbGetBlockSscDefault @SscGodTossing
    dbGetUndo   = dbGetUndoSscDefault @SscGodTossing
    dbGetHeader = dbGetHeaderSscDefault @SscGodTossing

instance MonadGState BlockTestMode where
    gsAdoptedBVData = gsAdoptedBVDataDefault
