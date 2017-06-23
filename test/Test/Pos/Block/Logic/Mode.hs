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
import           Mockable                (Production, currentTime, runProduction)
import           System.IO.Temp          (withSystemTempDirectory)
import           Test.QuickCheck         (Testable (..), ioProperty)
import           Test.QuickCheck.Monadic (PropertyM, monadic)

import           Pos.Block.Core          (Block, BlockHeader)
import           Pos.Block.Types         (Undo)
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
import           Pos.DB.DB               (closeNodeDBs, openNodeDBs)
import           Pos.DB.DB               (gsAdoptedBVDataDefault)
import qualified Pos.DB.GState           as GState
import           Pos.ExecMode            ((:::), ExecMode (..), ExecModeM, HasLens (..),
                                          modeContext)
import           Pos.Slotting            (MonadSlots (..), currentTimeSlottingSimple,
                                          getCurrentSlotBlockingSimple,
                                          getCurrentSlotInaccurateSimple,
                                          getCurrentSlotSimple)
import           Pos.Slotting.MemState   (MonadSlotsData (..), SlottingVar,
                                          getSlottingDataDefault, getSystemStartDefault,
                                          putSlottingDataDefault,
                                          waitPenultEpochEqualsDefault)
import           Pos.Ssc.Class           (SscBlock)
import           Pos.Ssc.GodTossing      (SscGodTossing)
import           Pos.Util.Util           (Some)

-- TODO: it shouldn't be 'Production', but currently we don't have anything else.
-- Expect some changes here somewhere in 2019.
type BaseMonad = Production

----------------------------------------------------------------------------
-- Main context
----------------------------------------------------------------------------

modeContext [d|
    data BlockTestContext = BlockTestContext
        { btcDBs         :: !(NodeDBs     ::: NodeDBs)
        , btcSlottingVar :: !(SlottingVar ::: SlottingVar)
        }
    |]

----------------------------------------------------------------------------
-- Initialization
----------------------------------------------------------------------------

modeContext [d|
    data InitCtx = InitCtx
        !(NodeDBs ::: NodeDBs)
    |]

data InitTag

instance HasLens InitTag InitCtx InitCtx where
    lensOf = identity

data BTestInit

type BlockTestInitMode = ExecMode BTestInit

type instance ExecModeM BTestInit = Mtl.ReaderT InitCtx BaseMonad

runBlockTestInitMode :: NodeDBs -> BlockTestInitMode a -> BaseMonad a
runBlockTestInitMode db (unExecMode -> action) = runReaderT action (InitCtx db)

-- Maybe we will make everything pure somewhere in 2021, but for now
-- this is commented out and let's use 'bracket'.
-- initBlockTestContext :: BlockTestContext
-- initBlockTestContext = ¯\_(ツ)_/¯

-- So here we go. Bracket, yes.
bracketBlockTestContext :: (BlockTestContext -> BaseMonad a) -> BaseMonad a
bracketBlockTestContext callback =
    withSystemTempDirectory "cardano-sl-testing" $ \dbPath ->
        bracket (openNodeDBs False dbPath) closeNodeDBs $ \btcDBs ->
            runBlockTestInitMode btcDBs $ do
                -- TODO: init DB
                systemStart <- Timestamp <$> currentTime
                slottingData <- GState.getSlottingData
                btcSlottingVar <- (systemStart, ) <$> newTVarIO slottingData
                ExecMode . lift $ callback BlockTestContext {..}

----------------------------------------------------------------------------
-- ExecMode
----------------------------------------------------------------------------

data BlockTestContextTag

instance HasLens BlockTestContextTag BlockTestContext BlockTestContext where
    lensOf = identity

data BTest

type BlockTestMode = ExecMode BTest

-- TODO: it shouldn't be 'BaseMonad', but currently we don't have anything else.
type instance ExecModeM BTest =
    Mtl.ReaderT BlockTestContext BaseMonad

unBlockTestMode :: ExecMode BTest a -> ExecModeM BTest a
unBlockTestMode = unExecMode

runBlockTestMode :: BlockTestMode a -> IO a
runBlockTestMode (unBlockTestMode -> action) =
    runProduction $ bracketBlockTestContext (runReaderT action)

----------------------------------------------------------------------------
-- Property
----------------------------------------------------------------------------

type BlockProperty = PropertyM BlockTestMode

instance Testable (BlockProperty a) where
    property blockProperty =
        property (monadic (ioProperty . runBlockTestMode) blockProperty)

----------------------------------------------------------------------------
-- Boilerplate instances
----------------------------------------------------------------------------

instance MonadDBRead BlockTestInitMode where
    dbGet = dbGetDefault
    dbIterSource = dbIterSourceDefault

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
