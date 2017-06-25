{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

{- |

The initialization mode is used to build the node context (node resources).
We need a separate mode for this because our initialization procedures require
database access, slotting, logging, etc.

The tricky part specific to this mode is the use of futures. Some parts of the
'InitModeContext' become available *during* initialization, so we create thunks
out of thin air and fill them as we go. This way 'InitMode' has all instances
it needs at all stages of initialization, but some of those instances can be
unusable until the relevant parts of the context are set.

-}

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
import           Pos.Context.Context   (GenesisStakes, GenesisUtxo)
import           Pos.Core              (IsHeader)
import           Pos.DB                (NodeDBs)
import           Pos.DB.Block          (MonadBlockDBWrite (..), dbGetBlockDefault,
                                        dbGetBlockSscDefault, dbGetHeaderDefault,
                                        dbGetHeaderSscDefault, dbGetUndoDefault,
                                        dbGetUndoSscDefault, dbPutBlundDefault)
import           Pos.DB.Class          (MonadBlockDBGeneric (..), MonadDB (..),
                                        MonadDBRead (..))
import           Pos.DB.Redirect       (dbDeleteDefault, dbGetDefault,
                                        dbIterSourceDefault, dbPutDefault,
                                        dbWriteBatchDefault)
import           Pos.ExecMode          ((:::), ExecMode (..), ExecModeM, modeContext)
import           Pos.Lrc.Context       (LrcContext)
import           Pos.Slotting.Class    (MonadSlots (..))
import           Pos.Slotting.Impl.Sum (SlottingContextSum, currentTimeSlottingSum,
                                        getCurrentSlotBlockingSum,
                                        getCurrentSlotInaccurateSum, getCurrentSlotSum)
import           Pos.Slotting.MemState (MonadSlotsData (..), SlottingVar,
                                        getSlottingDataDefault, getSystemStartDefault,
                                        putSlottingDataDefault,
                                        waitPenultEpochEqualsDefault)
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
        (GenesisStakes      ::: GenesisStakes)
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
    dbGet = dbGetDefault
    dbIterSource = dbIterSourceDefault

instance MonadDB (InitMode ssc) where
    dbPut = dbPutDefault
    dbWriteBatch = dbWriteBatchDefault
    dbDelete = dbDeleteDefault

instance SscHelpersClass ssc => MonadBlockDBWrite ssc (InitMode ssc) where
    dbPutBlund = dbPutBlundDefault

instance
    SscHelpersClass ssc =>
    MonadBlockDBGeneric (BlockHeader ssc) (Block ssc) Undo (InitMode ssc)
  where
    dbGetBlock  = dbGetBlockDefault @ssc
    dbGetUndo   = dbGetUndoDefault @ssc
    dbGetHeader = dbGetHeaderDefault @ssc

instance
    SscHelpersClass ssc =>
    MonadBlockDBGeneric (Some IsHeader) (SscBlock ssc) () (InitMode ssc)
  where
    dbGetBlock  = dbGetBlockSscDefault @ssc
    dbGetUndo   = dbGetUndoSscDefault @ssc
    dbGetHeader = dbGetHeaderSscDefault @ssc

instance MonadSlotsData (InitMode ssc) where
    getSystemStart = getSystemStartDefault
    getSlottingData = getSlottingDataDefault
    waitPenultEpochEquals = waitPenultEpochEqualsDefault
    putSlottingData = putSlottingDataDefault

instance MonadSlots (InitMode ssc) where
    getCurrentSlot = getCurrentSlotSum
    getCurrentSlotBlocking = getCurrentSlotBlockingSum
    getCurrentSlotInaccurate = getCurrentSlotInaccurateSum
    currentTimeSlotting = currentTimeSlottingSum

deriving instance HasLoggerName (InitMode ssc)
