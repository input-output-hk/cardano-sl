-- | MPC processing related workers.

module Pos.Worker.Mpc
       ( mpcOnNewSlot
       , mpcWorkers
       ) where

import           Control.Lens              (ix, (^?))
import           Control.TimeWarp.Logging  (logInfo)
import           Control.TimeWarp.Timed    (for, wait)
import           Formatting                (sformat, (%))
import           Universum

import           Pos.Communication.Methods (announceBlock)
import           Pos.Constants             (k, networkDiameter, slotDuration)
import           Pos.State                 (getLeaders)
import           Pos.Types                 (SlotId (..), slotIdF)
import           Pos.WorkMode              (WorkMode, getNodeContext, ncPublicKey)

-- | Action which should be done when new slot starts.
mpcOnNewSlot :: WorkMode m => SlotId -> m ()
mpcOnNewSlot slotId@SlotId {..} = do
    ourPk <- ncPublicKey <$> getNodeContext
    -- TODO: should we randomise sending times to avoid the situation when
    -- the network becomes overwhelmed with everyone's messages?
    when (siSlot == 0) $ do
        -- TODO: send a commitment
        notImplemented
    when (siSlot == 2 * k) $ do
        -- TODO: send an opening
        notImplemented
    when (siSlot == 4 * k) $ do
        -- TODO: send decrypted shares
        notImplemented

-- | All workers specific to MPC processing.
-- Exceptions:
-- 1. Worker which ticks when new slot starts.
mpcWorkers :: WorkMode m => [m ()]
mpcWorkers = []
