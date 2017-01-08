module Pos.Ssc.GodTossing.Richmen
       (
         gtLrcConsumer
       ) where

import           Universum

import           Pos.Constants            (k)
import qualified Pos.DB                   as DB
import           Pos.Ssc.Extra.Richmen    (MonadSscRichmen (..), isEmptySscRichmen)
import           Pos.Ssc.GodTossing.Types (SscGodTossing)
import           Pos.Types                (Coin, LrcConsumer (..), RichmenStake,
                                           RichmenStake, SlotId (..), mkCoin)
import           Pos.WorkMode             (WorkMode)

gtLrcConsumer :: WorkMode SscGodTossing m => LrcConsumer m
gtLrcConsumer = LrcConsumer
    {
      -- [CSL-93] Use eligibility threshold here
      lcThreshold = const (mkCoin 0)
    , lcIfNeedCompute = ifNeed
    , lcComputedCallback = onComputed
    , lcClearCallback = onClear
    , lcConsiderDelegated = False
    }

ifNeed :: WorkMode SscGodTossing m => SlotId -> m Bool
ifNeed SlotId{..} = do
    (epochIndex, richmen) <- DB.getGtRichmen
    isEmpty <- isEmptySscRichmen
    let needWrite = siSlot < k && siEpoch == epochIndex && isEmpty
    when needWrite $ writeSscRichmen (epochIndex, richmen)
    pure (siSlot < k && epochIndex < siEpoch)

onComputed :: WorkMode SscGodTossing m => SlotId -> Coin -> RichmenStake -> m ()
onComputed SlotId{..} _ richmen = do
    DB.putGtRichmen (siEpoch, richmen)
    writeSscRichmen (siEpoch, richmen)

onClear :: WorkMode SscGodTossing m => m ()
onClear = pass
