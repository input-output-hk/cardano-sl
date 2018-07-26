module Pos.Chain.Lrc.RichmenComponent
       ( RichmenComponent (..)
       ) where

import           Universum

import           Pos.Chain.Lrc.Types (FullRichmenData)
import           Pos.Core.Common (CoinPortion)

data RichmenComponent richmenData = RichmenComponent
    { rcToData            :: FullRichmenData -> richmenData
      -- ^ Converts 'FullRichmenData' to what needs to be saved.

    , rcTag               :: ByteString
      -- ^ Tag to identify component (short bytestring).

    , rcInitialThreshold  :: CoinPortion
      -- ^ Threshold used to deliminate richmen initially. Argument is
      -- the total system stake.

    , rcConsiderDelegated :: Bool
      -- ^ Whether to consider delegated stake.
    }
