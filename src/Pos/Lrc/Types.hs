-- | Types used in LRC.

module Pos.Lrc.Types
       ( Richmen
       , RichmenStake
       , toRichmen
       , FullRichmenData
       , LrcConsumer (..)
       ) where

import qualified Data.HashMap.Strict as HM
import           Data.List.NonEmpty  (NonEmpty)
import qualified Data.List.NonEmpty  as NE
import           Universum

import           Pos.Types           (Coin, EpochIndex, SlotId, StakeholderId)

-- | Addresses which have enough stake for participation in SSC.
type Richmen = NonEmpty StakeholderId

-- | Richmen with Stake
type RichmenStake = HashMap StakeholderId Coin

toRichmen :: RichmenStake -> Richmen
toRichmen =
    fromMaybe onNoRichmen . NE.nonEmpty . HM.keys
  where
    onNoRichmen = panic "There are no richmen!"

-- | Full richmen data consists of total stake at some point and stake
-- distribution among richmen.
type FullRichmenData = (Coin, RichmenStake)

-- | Datatype for LRC computation client.
-- If you want to compute richmen, you should add such client to LRC framework
data LrcConsumer m = LrcConsumer
    {
      lcThreshold         :: Coin -> Coin
    -- ^ Function which defines threshold depends on total stake
    , lcIfNeedCompute     :: SlotId -> m Bool
    -- ^ Function which defines necessity of richmen computation
    , lcComputedCallback  :: EpochIndex -> Coin -> RichmenStake -> m ()
    -- ^ Callback which will be called when richmen computed
    , lcClearCallback     :: m ()
    -- ^ Callback which will be called slot >= k
    , lcConsiderDelegated :: Bool
    -- ^ Whether delegated stake should be considered
    }
