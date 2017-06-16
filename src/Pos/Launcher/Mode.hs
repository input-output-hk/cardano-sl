{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}

module Pos.Launcher.Mode
    ( InitMode
    , runInitMode
    , InitModeContext(..)
    , newEmptyInitModeContext
    , putInitRef
    , InitRef(..)
    ) where

import           Universum

import           Control.Lens          (makeLensesFor)
import qualified Control.Monad.Reader  as Mtl
import           Ether.Internal        (HasLens (..))
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
import           Pos.ExecMode          (ExecMode (..), ExecModeM)
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

data InitRef a = InitRef
    { initRefVar :: MVar a
    , initRefVal :: a }

makeLensesFor [ ("initRefVal", "initRefValL") ] ''InitRef

newInitRef :: IO (InitRef a)
newInitRef = do
    m <- newEmptyMVar
    r <- unsafeInterleaveIO (readMVar m)
    pure (InitRef m r)

data InitModeContext = InitModeContext
    { imcNodeDBs         :: (InitRef NodeDBs)
    , imcGenesisUtxo     :: (InitRef GenesisUtxo)
    , imcGenesisLeaders  :: (InitRef GenesisLeaders)
    , imcNodeParams      :: (InitRef NodeParams)
    , imcSlottingVar     :: (InitRef SlottingVar)
    , imcSlottingContext :: (InitRef SlottingContextSum)
    , imcLrcContext      :: (InitRef LrcContext)
    }

newEmptyInitModeContext :: MonadIO m => m InitModeContext
newEmptyInitModeContext = liftIO $
    InitModeContext
        <$> newInitRef <*> newInitRef <*> newInitRef
        <*> newInitRef <*> newInitRef <*> newInitRef
        <*> newInitRef

makeLensesFor
    [ ( "imcNodeDBs", "imcNodeDBsL" )
    , ( "imcGenesisUtxo", "imcGenesisUtxoL")
    , ( "imcGenesisLeaders", "imcGenesisLeadersL")
    , ( "imcNodeParams", "imcNodeParamsL")
    , ( "imcSlottingVar", "imcSlottingVarL")
    , ( "imcSlottingContext", "imcSlottingContextL")
    , ( "imcLrcContext", "imcLrcContextL") ]
    ''InitModeContext

instance HasLens NodeDBs InitModeContext NodeDBs where
    lensOf = imcNodeDBsL . initRefValL

instance HasLens GenesisUtxo InitModeContext GenesisUtxo where
    lensOf = imcGenesisUtxoL . initRefValL

instance HasLens GenesisLeaders InitModeContext GenesisLeaders where
    lensOf = imcGenesisLeadersL . initRefValL

instance HasLens NodeParams InitModeContext NodeParams where
    lensOf = imcNodeParamsL . initRefValL

instance HasLens SlottingVar InitModeContext SlottingVar where
    lensOf = imcSlottingVarL . initRefValL

instance HasLens SlottingContextSum InitModeContext SlottingContextSum where
    lensOf = imcSlottingContextL . initRefValL

instance HasLens LrcContext InitModeContext LrcContext where
    lensOf = imcLrcContextL . initRefValL

data INIT ssc

type InitMode ssc = ExecMode (INIT ssc)

type instance ExecModeM (INIT ssc) =
    Mtl.ReaderT InitModeContext Production

runInitMode
    :: InitMode ssc a
    -> Production a
runInitMode act = do
    imc <- newEmptyInitModeContext
    Mtl.runReaderT (unExecMode act) imc

putInitRef :: (InitModeContext -> InitRef a) -> a -> InitMode ssc ()
putInitRef f a = do
    imc <- ExecMode Mtl.ask
    let InitRef v _ = f imc
    liftIO $ putMVar v a

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
