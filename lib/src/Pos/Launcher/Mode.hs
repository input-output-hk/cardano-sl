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

import           Control.Lens (makeLensesWith)
import qualified Control.Monad.Reader as Mtl
import           Mockable.Production (Production)

import           Pos.Core (HasConfiguration, Timestamp)
import           Pos.DB (NodeDBs)
import           Pos.DB.Block (dbGetSerBlockRealDefault, dbGetSerUndoRealDefault,
                               dbPutSerBlundsRealDefault)
import           Pos.DB.Class (MonadDB (..), MonadDBRead (..))
import           Pos.DB.Rocks (dbDeleteDefault, dbGetDefault, dbIterSourceDefault, dbPutDefault,
                               dbWriteBatchDefault)
import           Pos.Lrc.Context (LrcContext)
import           Pos.Slotting (HasSlottingVar (..))
import           Pos.Slotting.Class (MonadSlots (..))
import           Pos.Slotting.Impl (SimpleSlottingStateVar,
                                    currentTimeSlottingSimple,
                                    getCurrentSlotBlockingSimple,
                                    getCurrentSlotInaccurateSimple,
                                    getCurrentSlotSimple)
import           Pos.Slotting.MemState (MonadSlotsData)
import           Pos.Slotting.Types (SlottingData)
import           Pos.Util.Lens (postfixLFields)
import           Pos.Util.Util (HasLens (..))

-- The fields are lazy on purpose: this allows using them with
-- futures.
data InitModeContext = InitModeContext
    { imcNodeDBs            :: NodeDBs
    , imcSlottingVar        :: (Timestamp, TVar SlottingData)
    , imcSlottingStateVar   :: SimpleSlottingStateVar
    , imcLrcContext         :: LrcContext
    }

makeLensesWith postfixLFields ''InitModeContext

type InitMode = Mtl.ReaderT InitModeContext Production

runInitMode :: InitModeContext -> InitMode a -> Production a
runInitMode = flip Mtl.runReaderT

instance HasLens NodeDBs InitModeContext NodeDBs where
    lensOf = imcNodeDBs_L

instance HasLens SimpleSlottingStateVar InitModeContext SimpleSlottingStateVar where
    lensOf = imcSlottingStateVar_L

instance HasLens LrcContext InitModeContext LrcContext where
    lensOf = imcLrcContext_L

instance HasSlottingVar InitModeContext where
    slottingTimestamp = imcSlottingVar_L . _1
    slottingVar = imcSlottingVar_L . _2

instance HasConfiguration => MonadDBRead InitMode where
    dbGet = dbGetDefault
    dbIterSource = dbIterSourceDefault
    dbGetSerBlock = dbGetSerBlockRealDefault
    dbGetSerUndo = dbGetSerUndoRealDefault

instance HasConfiguration => MonadDB InitMode where
    dbPut = dbPutDefault
    dbWriteBatch = dbWriteBatchDefault
    dbDelete = dbDeleteDefault
    dbPutSerBlunds = dbPutSerBlundsRealDefault

instance (HasConfiguration, MonadSlotsData ctx InitMode) =>
         MonadSlots ctx InitMode
  where
    getCurrentSlot           = getCurrentSlotSimple
    getCurrentSlotBlocking   = getCurrentSlotBlockingSimple
    getCurrentSlotInaccurate = getCurrentSlotInaccurateSimple
    currentTimeSlotting      = currentTimeSlottingSimple
