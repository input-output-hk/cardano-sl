{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Pos.Launcher.Mode
    ( InitMode
    , runInitMode
    , InitModeContext(..)
    , newInitFuture
    ) where

import           Universum

import qualified Control.Monad.Reader  as Mtl
import           Mockable.Production   (Production)
import           System.IO.Unsafe      (unsafeInterleaveIO)
import           System.Wlog           (HasLoggerName (..))

import           Pos.Block.Core        (Block, BlockHeader)
import           Pos.Block.Types       (Undo)
import           Pos.Context.Context   (GenesisLeaders, GenesisUtxo, NodeParams)
import           Pos.Core              (IsHeader)
import           Pos.DB                (NodeDBs)
import           Pos.DB.Block          (MonadBlockDBWrite (..), dbGetBlockReal,
                                        dbGetBlockReal', dbGetHeaderReal,
                                        dbGetHeaderReal', dbGetUndoReal, dbGetUndoReal',
                                        dbPutBlundReal)
import           Pos.DB.Class          (MonadBlockDBGeneric (..), MonadDB (..),
                                        MonadDBRead (..))
import           Pos.DB.Redirect       (dbDeleteReal, dbGetReal, dbPutReal,
                                        dbWriteBatchReal)
import           Pos.ExecMode          (ExecMode (..), ExecModeM, modeContext, (:::))
import           Pos.Lrc.Context       (LrcContext)
import           Pos.Slotting.Class    (MonadSlots (..))
import           Pos.Slotting.Impl.Sum (SlottingContextSum, currentTimeSlottingReal,
                                        getCurrentSlotBlockingReal,
                                        getCurrentSlotInaccurateReal, getCurrentSlotReal)
import           Pos.Slotting.MemState (MonadSlotsData (..), SlottingVar,
                                        getSlottingDataReal, getSystemStartReal,
                                        putSlottingDataReal, waitPenultEpochEqualsReal)
import           Pos.Ssc.Class.Helpers (SscHelpersClass)
import           Pos.Ssc.Class.Types   (SscBlock)
import           Pos.Util              (Some (..))

-- | 'newInitFuture' creates a thunk and a procedure to fill it. This can be
-- used to create a data structure and initialize it gradually while doing some
-- IO (e.g. accessing the database).
-- There are two contracts the caller must obey:
-- * the thunk isn't forced until the procedure to fill it was called.
--   Violation of this contract will lead to an error:
--     "thread blocked indefinitely in an MVar operation".
-- * the procedure to fill the thunk is called at most one time or multiple
--   times but with equivalent values.  Violation of this contract will lead
--   to non-deterministic choice of which value will be used.
newInitFuture :: (MonadIO m, MonadIO m') => m (a, a -> m' ())
newInitFuture = do
    v <- newEmptyMVar
    r <- liftIO $ unsafeInterleaveIO (readMVar v)
    pure (r, putMVar v)

modeContext [d|
    -- The fields are lazy on purpose: this allows using them with
    -- futures.
    data InitModeContext = InitModeContext
        (NodeDBs            ::: NodeDBs)
        (GenesisUtxo        ::: GenesisUtxo)
        (GenesisLeaders     ::: GenesisLeaders)
        (NodeParams         ::: NodeParams)
        (SlottingVar        ::: SlottingVar)
        (SlottingContextSum ::: SlottingContextSum)
        (LrcContext         ::: LrcContext)
    |]

data INIT ssc

type InitMode ssc = ExecMode (INIT ssc)

type instance ExecModeM (INIT ssc) =
    Mtl.ReaderT InitModeContext Production

runInitMode :: InitModeContext -> InitMode ssc a -> Production a
runInitMode imc act = Mtl.runReaderT (unExecMode act) imc

instance MonadDBRead (InitMode ssc) where
    dbGet = dbGetReal

instance MonadDB (InitMode ssc) where
    dbPut = dbPutReal
    dbWriteBatch = dbWriteBatchReal
    dbDelete = dbDeleteReal

instance SscHelpersClass ssc => MonadBlockDBWrite ssc (InitMode ssc) where
    dbPutBlund = dbPutBlundReal

instance
    SscHelpersClass ssc =>
    MonadBlockDBGeneric (BlockHeader ssc) (Block ssc) Undo (InitMode ssc)
  where
    dbGetBlock  = dbGetBlockReal @ssc
    dbGetUndo   = dbGetUndoReal @ssc
    dbGetHeader = dbGetHeaderReal @ssc

instance
    SscHelpersClass ssc =>
    MonadBlockDBGeneric (Some IsHeader) (SscBlock ssc) () (InitMode ssc)
  where
    dbGetBlock  = dbGetBlockReal' @ssc
    dbGetUndo   = dbGetUndoReal' @ssc
    dbGetHeader = dbGetHeaderReal' @ssc

instance MonadSlotsData (InitMode ssc) where
    getSystemStart = getSystemStartReal
    getSlottingData = getSlottingDataReal
    waitPenultEpochEquals = waitPenultEpochEqualsReal
    putSlottingData = putSlottingDataReal

instance MonadSlots (InitMode ssc) where
    getCurrentSlot = getCurrentSlotReal
    getCurrentSlotBlocking = getCurrentSlotBlockingReal
    getCurrentSlotInaccurate = getCurrentSlotInaccurateReal
    currentTimeSlotting = currentTimeSlottingReal

deriving instance HasLoggerName (InitMode ssc)
