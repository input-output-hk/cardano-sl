-- | MPC processing related workers.

module Pos.Worker.Mpc
       ( mpcOnNewSlot
       , mpcWorkers
       ) where

import           Control.TimeWarp.Logging (logDebug)
import           Universum

import           Formatting               (build, sformat, (%))
import           Pos.Communication.Types  (SendCommitment (..), SendOpening (..),
                                           SendShares (..))
import           Pos.Constants            (k)
import           Pos.DHT                  (sendToNeighbors)
import           Pos.State                (generateNewSecret, getOurOpening, getOurShares)
import           Pos.Types                (SlotId (..))
import           Pos.WorkMode             (WorkMode, getNodeContext, ncPublicKey,
                                           ncSecretKey, ncVssKeyPair)

-- | Action which should be done when new slot starts.
mpcOnNewSlot :: WorkMode m => SlotId -> m ()
mpcOnNewSlot SlotId {..} = do
    ourPk <- ncPublicKey <$> getNodeContext
    ourSk <- ncSecretKey <$> getNodeContext
    -- TODO: should we randomise sending times to avoid the situation when
    -- the network becomes overwhelmed with everyone's messages?

    -- Generate a new commitment and opening for MPC; send the commitment.
    when (siSlot == 0) $ do
        logDebug $ sformat ("Generating secret for " % build % "th epoch") siEpoch
        (comm, _) <- generateNewSecret ourSk siEpoch
        () <$ sendToNeighbors (SendCommitment ourPk comm)
    -- Send the opening
    when (siSlot == 2 * k) $ do
        mbOpen <- getOurOpening
        whenJust mbOpen $ \open ->
            void . sendToNeighbors $ SendOpening ourPk open
    -- Send decrypted shares that others have sent us
    when (siSlot == 4 * k) $ do
        ourVss <- ncVssKeyPair <$> getNodeContext
        shares <- getOurShares ourVss
        void . sendToNeighbors $ SendShares ourPk shares

    -- | All workers specific to MPC processing.
-- Exceptions:
-- 1. Worker which ticks when new slot starts.
mpcWorkers :: [a]
mpcWorkers = []
