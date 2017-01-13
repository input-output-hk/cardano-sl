module Pos.Genesis.Types
       ( StakeDistribution (..)
       , GenesisData (..)
       ) where

import           Universum

import           Pos.Ssc.GodTossing.Types.Base (VssCertificatesMap)
import           Pos.Types.Types               (Address, Coin)

-- | Stake distribution in genesis block.
-- FlatStakes is a flat distribution, i. e. each node has the same amount of coins.
-- BitcoinStakes is a Bitcoin mining pool-style ditribution.
data StakeDistribution
    = FlatStakes !Word     -- number of stakeholders
                 !Coin     -- total number of coins
    | BitcoinStakes !Word  -- number of stakeholders
                    !Coin  -- total number of coins
    | TestnetStakes
        { sdTotalStake :: !Coin
        , sdRichmen    :: !Word
        , sdPoor       :: !Word
        }

-- | Hardcoded genesis data
data GenesisData = GenesisData
    { gdAddresses       :: [Address]
    , gdDistribution    :: StakeDistribution
    , gdVssCertificates :: VssCertificatesMap
    }
