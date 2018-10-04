module Cardano.Wallet.Kernel.Restore.Types where

import           Universum

import           Data.Time
import           Formatting (bprint, build, formatToString, (%))
import qualified Formatting.Buildable
import qualified Prelude

import qualified Cardano.Wallet.Kernel.DB.Spec.Update as Spec
import           Pos.Chain.Block (HeaderHash)

{-------------------------------------------------------------------------------
  Timing information (for throughput calculations)
-------------------------------------------------------------------------------}

-- | Keep track of how many events have happened since a given start time.
data TimingData
  = NoTimingData
  | Timing Integer UTCTime

-- | A rate, represented as an event count over a time interval.
data Rate = Rate Integer NominalDiffTime

-- | Log an event; once k' events have been seen, return the event rate
-- and start the count over again.
tickTiming :: Integer -> TimingData -> IO (Maybe Rate, TimingData)
tickTiming _  NoTimingData     = (Nothing,) . Timing 0 <$> getCurrentTime
tickTiming k' (Timing k start)
  | k == k' = do
        now <- getCurrentTime
        let rate = Rate k (now `diffUTCTime` start)
        return (Just rate, Timing 0 now)
  | otherwise = return (Nothing, Timing (k + 1) start)

-- | Convert a rate to a number of events per second.
perSecond :: Rate -> Word64
perSecond (Rate n dt) = fromInteger $ round (toRational n / toRational dt)

{-------------------------------------------------------------------------------
  Exceptions
-------------------------------------------------------------------------------}

-- | Exception during restoration
data RestorationException =
    RestorationBlockNotFound HeaderHash
  | RestorationSuccessorNotFound HeaderHash
  | RestorationUndoNotFound HeaderHash
  | RestorationApplyHistoricalBlockFailed Spec.ApplyBlockFailed
  | RestorationFinishUnreachable HeaderHash HeaderHash

instance Buildable RestorationException where
    build (RestorationBlockNotFound hash) =
      bprint ("RestorationBlockNotFound " % build) hash
    build (RestorationSuccessorNotFound hash) =
      bprint ("RestorationSuccessorNotFound " % build) hash
    build (RestorationUndoNotFound hash) =
      bprint ("RestorationUndoNotFound " % build) hash
    build (RestorationApplyHistoricalBlockFailed err) =
      bprint ("RestorationApplyHistoricalBlockFailed " % build) err
    build (RestorationFinishUnreachable target final) =
      bprint ("RestorationFinishUnreachable " % build % " " % build) target final

instance Show RestorationException where
    show = formatToString build

instance Exception RestorationException

