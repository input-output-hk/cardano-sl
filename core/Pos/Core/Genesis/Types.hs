module Pos.Core.Genesis.Types
       ( StakeDistribution (..)
       , GenesisCoreData (..)
       , getTotalStake
       ) where

import           Universum

import           Data.Default       (Default (..))

import           Pos.Core.Coin      (coinToInteger, sumCoins, unsafeAddCoin,
                                     unsafeIntegerToCoin, unsafeMulCoin)
import           Pos.Core.Constants (genesisKeysN)
import           Pos.Core.Types     (Address, Coin, StakeholderId, mkCoin)

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
    -- First three nodes get 0.875% of stake.
    | ExponentialStakes
    -- ExplicitStakes is basically just 'Utxo'. Except that we can't use
    -- TxOutDistribution here (it's defined in txp/) and instead we use
    -- @[(StakeholderId, Coin)]@.
    | ExplicitStakes !(HashMap Address (Coin, [(StakeholderId, Coin)]))
    | CombinedStakes StakeDistribution StakeDistribution
    deriving (Show, Eq)

instance Monoid StakeDistribution where
    mempty = FlatStakes 0 (mkCoin 0)
    mappend = CombinedStakes

instance Default StakeDistribution where
    def = FlatStakes genesisKeysN
              (mkCoin 10000 `unsafeMulCoin` (genesisKeysN :: Int))

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
data GenesisCoreData = GenesisCoreData
    { gcdAddresses         :: [Address]
    , gcdDistribution      :: StakeDistribution
    , gcdBootstrapBalances :: !(HashMap StakeholderId Coin)
    }
    deriving (Show, Eq)

instance Monoid GenesisCoreData where
    mempty = GenesisCoreData mempty mempty mempty
    (GenesisCoreData addrsA distA bbsA) `mappend` (GenesisCoreData addrsB distB bbsB) =
        GenesisCoreData (addrsA <> addrsB) (distA <> distB) (bbsA <> bbsB)
