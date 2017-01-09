module Pos.Ssc.GodTossing.Richmen
       (
         gtLrcConsumer
       ) where

import           Universum

import           Pos.Constants            (k)
import qualified Pos.DB                   as DB
import           Pos.Lrc.Types            (LrcConsumer (..), RichmenStake, RichmenStake)
import           Pos.Ssc.Extra.Richmen    (MonadSscRichmen (..), isEmptySscRichmen)
import           Pos.Ssc.GodTossing.Types (SscGodTossing)
import           Pos.Types                (Coin, SlotId (..), mkCoin)
import           Pos.WorkMode             (NewWorkMode)

-- | Consumer will be called on every Richmen computation.
gtLrcConsumer :: NewWorkMode SscGodTossing m => LrcConsumer m
gtLrcConsumer = LrcConsumer
    {
      -- [CSL-93] Use eligibility threshold here
      lcThreshold = const (mkCoin 0)
    , lcIfNeedCompute = ifNeed
    , lcComputedCallback = onComputed
    , lcClearCallback = onClear
    , lcConsiderDelegated = False
    }

-- | Returns True if cached value isn't corresponds for current epoch
ifNeed :: NewWorkMode SscGodTossing m => SlotId -> m Bool
ifNeed SlotId{..} = do
    (epochIndex, richmen) <- DB.getGtRichmen
    isEmpty <- isEmptySscRichmen
    let needWrite = siSlot < k && siEpoch == epochIndex && isEmpty
    when needWrite $ writeSscRichmen (epochIndex, richmen)
    pure (siSlot < k && epochIndex < siEpoch)

-- | Store computed value into DB and cache.
onComputed :: NewWorkMode SscGodTossing m => SlotId -> Coin -> RichmenStake -> m ()
onComputed SlotId{..} _ richmen = do
    DB.putGtRichmen (siEpoch, richmen)
    writeSscRichmen (siEpoch, richmen)

-- | Do nothing on clear
onClear :: NewWorkMode SscGodTossing m => m ()
onClear = pass
