module Pos.Ssc.GodTossing.Richmen
       (
         gtLrcConsumer
       ) where

import           System.Wlog              (logWarning)
import           Universum

import           Pos.Constants            (k)
import qualified Pos.DB                   as DB
import           Pos.Ssc.Extra.Richmen    (MonadSscRichmen (..))
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
    when needWrite $ writeSscRichmen richmen
    pure (siSlot < k && epochIndex < siEpoch)

onComputed :: WorkMode SscGodTossing m => SlotId -> Coin -> RichmenStake -> m ()
onComputed SlotId{..} _ richmen = do
    wasFull <- clearSscRichmen
    when wasFull $ logWarning $ "SSC richmen isn't cleared when onComputed called"
    DB.putGtRichmen siEpoch richmen
    writeSscRichmen richmen

onClear :: WorkMode SscGodTossing m => m ()
onClear = void clearSscRichmen
