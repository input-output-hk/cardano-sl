module Pos.Core.Genesis.Types
       ( StakeDistribution (..)
       , GenesisCoreData (..)
       , getTotalStake

       -- compatibility
       , GenesisCoreData0 (..)
       , toGenesisCoreData
       ) where

import           Universum

import           Data.Default       (Default (..))

import           Pos.Core.Coin      (coinToInteger, sumCoins, unsafeAddCoin,
                                     unsafeIntegerToCoin, unsafeMulCoin)
import           Pos.Core.Constants (genesisKeysN)
import           Pos.Core.Types     (Address, Coin, StakeholderId, mkCoin)
import           Pos.Util.Util      (getKeys)

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
    mappend a b
        | a == mempty = b
        | b == mempty = a
        | otherwise = CombinedStakes a b

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

-- | Hardcoded genesis data.
data GenesisCoreData = GenesisCoreData
    { gcdAddresses             :: !([Address])
    , gcdDistribution          :: !StakeDistribution
    , gcdBootstrapStakeholders :: !(HashSet StakeholderId)
      -- ^ Bootstrap era stakeholders.
    }
    deriving (Show, Eq)

instance Monoid GenesisCoreData where
    mempty = GenesisCoreData mempty mempty mempty
    (GenesisCoreData addrsA distA bbsA) `mappend` (GenesisCoreData addrsB distB bbsB) =
        GenesisCoreData (addrsA <> addrsB) (distA <> distB) (bbsA <> bbsB)

----------------------------------------------------------------------------
-- Compatibility
----------------------------------------------------------------------------

-- | Compatibility datatype for 'GenesisCoreData'
data GenesisCoreData0 = GenesisCoreData0
    { _0gcdAddresses         :: !([Address])
    , _0gcdDistribution      :: !StakeDistribution
    , _0gcdBootstrapBalances :: !(HashMap StakeholderId Coin)
      -- ^ Bootstrap era addresses.
    }
    deriving (Show, Eq)

toGenesisCoreData :: GenesisCoreData0 -> GenesisCoreData
toGenesisCoreData (GenesisCoreData0 a b c) = GenesisCoreData a b (getKeys c)
