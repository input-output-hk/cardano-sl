module Pos.Ssc.GodTossing.Richmen
       ( gtLrcConsumer
       ) where

import           Universum

import qualified Pos.DB                   as DB
import           Pos.Lrc.Types            (LrcConsumer (..), RichmenStake, RichmenStake)
import           Pos.Ssc.Extra.Richmen    (MonadSscRichmen (..), isEmptySscRichmen)
import           Pos.Ssc.GodTossing.Types (SscGodTossing)
import           Pos.Types                (Coin, EpochIndex, mkCoin)
import           Pos.WorkMode             (WorkMode)

-- | Consumer will be called on every Richmen computation.
gtLrcConsumer :: WorkMode SscGodTossing m => LrcConsumer m
gtLrcConsumer = LrcConsumer
    {
      -- [CSL-93] Use eligibility threshold here
      lcThreshold = const (mkCoin 0)
    , lcIfNeedCompute = ifNeed
    , lcComputedCallback = onComputed
    , lcConsiderDelegated = False
    }

-- Returns True if cached value doesnt't correspond to current epoch
ifNeed
    :: WorkMode SscGodTossing m
    => EpochIndex -> m Bool
ifNeed toCompute = do
    (epochIndex, richmen) <- DB.getGtRichmen
    isEmpty <- isEmptySscRichmen
    let needWrite = toCompute == epochIndex && isEmpty
    when needWrite $ writeSscRichmen (epochIndex, richmen)
    pure (epochIndex < toCompute)

-- Store computed value into DB and cache.
onComputed :: WorkMode SscGodTossing m => EpochIndex -> Coin -> RichmenStake -> m ()
onComputed epoch _ richmen = do
    DB.putGtRichmen (epoch, richmen)
    writeSscRichmen (epoch, richmen)
