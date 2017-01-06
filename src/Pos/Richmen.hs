module Pos.Richmen
       (
       ) where

import           Universum

import           Pos.Types    (Coin, CoinPortion, StakeholderId)
import           Pos.WorkMode (WorkMode)

data LrcConsumer m = LrcConsumer
    {
      lcThreshold         :: CoinPortion
    , lcComputedCallback  :: Coin -> HashMap StakeholderId Coin -> m ()
    , lcClearCallback     :: m ()
    , lcConsiderDelegated :: Bool
    }

allLrcComsumers :: WorkMode ssc m => [LrcConsumer m]
allLrcComsumers = []
