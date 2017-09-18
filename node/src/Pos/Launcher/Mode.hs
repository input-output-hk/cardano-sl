{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS -fno-warn-unused-top-binds #-} -- for lenses

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
       ) where

import           Universum

import           Control.Lens            (makeLensesWith)
import qualified Control.Monad.Reader    as Mtl
import           Ether.Internal          (HasLens (..))
import           Mockable.Production     (Production)

import           Pos.Block.Core          (Block, BlockHeader)
import           Pos.Block.Types         (Undo)
import           Pos.Core                (HasConfiguration, IsHeader, Timestamp)
import           Pos.DB                  (NodeDBs)
import           Pos.DB.Block            (dbGetBlockDefault, dbGetBlockSscDefault,
                                          dbGetHeaderDefault, dbGetHeaderSscDefault,
                                          dbGetUndoDefault, dbGetUndoSscDefault,
                                          dbPutBlundDefault)
import           Pos.DB.Class            (MonadBlockDBGeneric (..),
                                          MonadBlockDBGenericWrite (..), MonadDB (..),
                                          MonadDBRead (..))
import           Pos.DB.Rocks            (dbDeleteDefault, dbGetDefault,
                                          dbIterSourceDefault, dbPutDefault,
                                          dbWriteBatchDefault)
import           Pos.Infra.Configuration (HasInfraConfiguration)
import           Pos.Lrc.Context         (LrcContext)
import           Pos.Slotting            (HasSlottingVar (..), SlottingData)
import           Pos.Slotting.Class      (MonadSlots (..))
import           Pos.Slotting.Impl.Sum   (SlottingContextSum, currentTimeSlottingSum,
                                          getCurrentSlotBlockingSum,
                                          getCurrentSlotInaccurateSum, getCurrentSlotSum)
import           Pos.Slotting.MemState   (MonadSlotsData)
import           Pos.Ssc.Class.Helpers   (SscHelpersClass)
import           Pos.Ssc.Class.Types     (SscBlock)
import           Pos.Util                (Some (..))
import           Pos.Util.Util           (postfixLFields)


-- The fields are lazy on purpose: this allows using them with
-- futures.
data InitModeContext ssc = InitModeContext
    { imcNodeDBs            :: NodeDBs
    , imcSlottingVar        :: (Timestamp, TVar SlottingData)
    , imcSlottingContextSum :: SlottingContextSum
    , imcLrcContext         :: LrcContext
    }

makeLensesWith postfixLFields ''InitModeContext

type InitMode ssc = Mtl.ReaderT (InitModeContext ssc) Production

runInitMode :: InitModeContext ssc -> InitMode ssc a -> Production a
runInitMode = flip Mtl.runReaderT

instance HasLens NodeDBs (InitModeContext ssc) NodeDBs where
    lensOf = imcNodeDBs_L

instance HasLens SlottingContextSum (InitModeContext ssc) SlottingContextSum where
    lensOf = imcSlottingContextSum_L

instance HasLens LrcContext (InitModeContext ssc) LrcContext where
    lensOf = imcLrcContext_L

instance HasSlottingVar (InitModeContext ssc) where
    slottingTimestamp = imcSlottingVar_L . _1
    slottingVar = imcSlottingVar_L . _2

instance HasConfiguration => MonadDBRead (InitMode ssc) where
    dbGet = dbGetDefault
    dbIterSource = dbIterSourceDefault

instance HasConfiguration => MonadDB (InitMode ssc) where
    dbPut = dbPutDefault
    dbWriteBatch = dbWriteBatchDefault
    dbDelete = dbDeleteDefault

instance
    (HasConfiguration, SscHelpersClass ssc) =>
    MonadBlockDBGeneric (BlockHeader ssc) (Block ssc) Undo (InitMode ssc)
  where
    dbGetBlock  = dbGetBlockDefault @ssc
    dbGetUndo   = dbGetUndoDefault @ssc
    dbGetHeader = dbGetHeaderDefault @ssc

instance (HasConfiguration, SscHelpersClass ssc) =>
         MonadBlockDBGenericWrite (BlockHeader ssc) (Block ssc) Undo (InitMode ssc) where
    dbPutBlund = dbPutBlundDefault

instance
    (HasConfiguration, SscHelpersClass ssc) =>
    MonadBlockDBGeneric (Some IsHeader) (SscBlock ssc) () (InitMode ssc)
  where
    dbGetBlock  = dbGetBlockSscDefault @ssc
    dbGetUndo   = dbGetUndoSscDefault @ssc
    dbGetHeader = dbGetHeaderSscDefault @ssc

instance (HasConfiguration, HasInfraConfiguration, MonadSlotsData ctx (InitMode ssc)) =>
         MonadSlots ctx (InitMode ssc)
  where
    getCurrentSlot           = getCurrentSlotSum
    getCurrentSlotBlocking   = getCurrentSlotBlockingSum
    getCurrentSlotInaccurate = getCurrentSlotInaccurateSum
    currentTimeSlotting      = currentTimeSlottingSum

