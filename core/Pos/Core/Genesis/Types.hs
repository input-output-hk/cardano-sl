-- | Types related to genesis core data.

module Pos.Core.Genesis.Types
       ( StakeDistribution (..)
       , getTotalStake

       , AddrDistribution
       , GenesisCoreData (..)
       , mkGenesisCoreData
       , concatGenesisCoreData

       -- compatibility
       , GenesisCoreData0 (..)
       , toGenesisCoreData
       ) where

import           Universum

import           Pos.Core.Coin  (coinToInteger, sumCoins, unsafeIntegerToCoin)
import           Pos.Core.Types (Address, Coin, StakeholderId, mkCoin)

-- | Stake distribution in genesis block.
data StakeDistribution
    -- | FlatStakes is a flat distribution, i. e. each node has the
    -- same amount of coins.
    = FlatStakes !Word     -- ^ Number of stakeholders
                 !Coin     -- ^ Total number of coins
    -- | Distribution mimicking bitcoin mining pool style.
    | BitcoinStakes !Word  -- ^ Number of stakeholders
                    !Coin  -- ^ Total number of coins
    -- | Rich/poor distribution, for testnet mostly.
    | RichPoorStakes
        { sdRichmen   :: !Word
        , sdRichStake :: !Coin
        , sdPoor      :: !Word
        , sdPoorStake :: !Coin
        }
    -- | First three nodes get 0.875% of stake.
    | ExponentialStakes
    -- | Custom stakes list.
    | CustomStakes [Coin]
    deriving (Show, Eq)

-- | Gets total amount of stake and addresses of distribution.
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
getTotalStake (CustomStakes balances) =
    unsafeIntegerToCoin $ sumCoins balances

-- | List of distributions accompained by related addresses set (what
-- to distribute and how).
type AddrDistribution = [(HashSet Address, StakeDistribution)]

-- | Hardcoded genesis data to generate utxo from.
data GenesisCoreData = UnsafeGenesisCoreData
    { gcdAddrDistribution      :: !AddrDistribution
      -- ^ Address distribution. Determines utxo without boot
      -- stakeholders distribution (addresses and coins).
    , gcdBootstrapStakeholders :: !(HashSet StakeholderId)
      -- ^ Bootstrap era stakeholders.
    }
    deriving (Show, Eq)

-- | Safe constructor for 'GenesisCoreData'. Throws error if something is wrong.
mkGenesisCoreData ::
       [(HashSet Address, StakeDistribution)]
    -> HashSet StakeholderId
    -> Either String GenesisCoreData
mkGenesisCoreData distribution bootStakeholders = do
    -- TODO add checks
    -- 1. Every set of address matches by the size to distribution size
    -- 2. If using CustomStakes, lengths match
    -- 3. (?) Addresses in (map fst) are unique
    -- 4. All the stake is distributed to gcdBootstrapStakeholders
    pure $ UnsafeGenesisCoreData distribution bootStakeholders

-- | Concats 'GenesisCoreData'. We do not use 'Monoid' instance for
-- 'GenesisCoreData' because 'mempty' doesn't make sense (empty boot
-- stakeholders?).
concatGenesisCoreData ::
       GenesisCoreData -> GenesisCoreData -> Either String GenesisCoreData
concatGenesisCoreData
    (UnsafeGenesisCoreData distA bsA)
    (UnsafeGenesisCoreData distB bsB) =
        let distrs = distA <> distB
            bootStakeholders = bsA <> bsB
        in mkGenesisCoreData distrs bootStakeholders

----------------------------------------------------------------------------
-- Compatibility
----------------------------------------------------------------------------

-- | Compatibility datatype for 'GenesisCoreData'
data GenesisCoreData0 = GenesisCoreData0
    { _0gcdAddresses         :: !([Address])
    , _0gcdDistribution      :: !StakeDistribution
    , _0gcdBootstrapBalances :: !(HashMap StakeholderId Coin)
    }
    deriving (Show, Eq)

toGenesisCoreData :: GenesisCoreData0 -> GenesisCoreData
toGenesisCoreData _ =
    error $ "toGenesisCoreData: old format is no longer " <>
            "supported. Kick @volhovm to delete this code"
