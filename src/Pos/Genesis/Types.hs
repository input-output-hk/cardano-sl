module Pos.Genesis.Types
       ( StakeDistribution (..)
       , GenesisData (..)
       , getTotalStake
       ) where

import           Universum

import           Pos.Core           (Address, Coin, StakeholderId, coinToInteger, mkCoin,
                                     sumCoins, unsafeAddCoin, unsafeIntegerToCoin)
import           Pos.Txp.Core.Types (TxOutDistribution)

-- | Stake distribution in genesis block.
-- FlatStakes is a flat distribution, i. e. each node has the same amount of coins.
-- BitcoinStakes is a Bitcoin mining pool-style ditribution.
data StakeDistribution
    = FlatStakes !Word     -- number of stakeholders
                 !Coin     -- total number of coins
    | BitcoinStakes !Word  -- number of stakeholders
                    !Coin  -- total number of coins
    | RichPoorStakes
        { sdRichmen   :: !Word
        , sdRichStake :: !Coin
        , sdPoor      :: !Word
        , sdPoorStake :: !Coin
        }
    | ExponentialStakes -- First three nodes get 0.875% of stake.
    | ExplicitStakes !(HashMap Address (Coin, TxOutDistribution))
    | CombinedStakes StakeDistribution StakeDistribution
    deriving (Show, Eq)

instance Monoid StakeDistribution where
    mempty = FlatStakes 0 (mkCoin 0)
    mappend = CombinedStakes

getTotalStake :: StakeDistribution -> Coin
getTotalStake (FlatStakes _ st) = st
getTotalStake (BitcoinStakes _ st) = st
getTotalStake RichPoorStakes {..} = unsafeIntegerToCoin $
    coinToInteger sdRichStake * fromIntegral sdRichmen +
    coinToInteger sdPoorStake * fromIntegral sdPoor
getTotalStake ExponentialStakes = mkCoin . sum $
    let g 0 = []
        g n = n : g (n `div` 2)
    in g 5000
getTotalStake (ExplicitStakes balances) = unsafeIntegerToCoin $
    sumCoins $ fst <$> balances
getTotalStake (CombinedStakes st1 st2) =
    getTotalStake st1 `unsafeAddCoin` getTotalStake st2

-- | Hardcoded genesis data
data GenesisData = GenesisData
    { gdAddresses         :: [Address]
    , gdDistribution      :: StakeDistribution
    , gdBootstrapBalances :: !(HashMap StakeholderId Coin)
    }
    deriving (Show, Eq)

instance Monoid GenesisData where
    mempty = GenesisData mempty mempty mempty
    (GenesisData addrsA distA bbsA) `mappend` (GenesisData addrsB distB bbsB) =
        GenesisData (addrsA <> addrsB) (distA <> distB) (bbsA <> bbsB)
