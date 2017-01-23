-- | Types used in LRC.

module Pos.Lrc.Types
       ( Richmen
       , RichmenSet
       , RichmenStake
       , toRichmen
       , FullRichmenData
       ) where

import qualified Data.HashMap.Strict as HM
import           Universum

import           Pos.Types           (Coin, StakeholderId)

-- | Addresses which have enough stake for participation in SSC.
type Richmen = NonEmpty StakeholderId

-- | Hashset of richmen.
type RichmenSet = HashSet StakeholderId

-- | Richmen with Stake
type RichmenStake = HashMap StakeholderId Coin

toRichmen :: RichmenStake -> Richmen
toRichmen =
    fromMaybe onNoRichmen . nonEmpty . HM.keys
  where
    onNoRichmen = panic "There are no richmen!"

-- | Full richmen data consists of total stake at some point and stake
-- distribution among richmen.
type FullRichmenData = (Coin, RichmenStake)
