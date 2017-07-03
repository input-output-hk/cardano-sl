-- | Types related to genesis core data.

module Pos.Core.Genesis.Types
       ( StakeDistribution (..)
       , getTotalStake

       , AddrDistribution
       , GenesisCoreData (..)
       , mkGenesisCoreData
       ) where

import           Universum

import           Pos.Core.Coin  (coinToInteger, sumCoins, unsafeIntegerToCoin)
import           Pos.Core.Types (Address, Coin, StakeholderId, mkCoin)

-- | Balances distribution in genesis block.
--
-- TODO Needs a proper name (maybe "BalancesDistribution"), see
-- CSL-1124.
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
    -- | First three nodes get 0.875% of balance.
    -- TODO Doesn't have explicit length, would be nice to have.
    | ExponentialStakes
    -- | Custom balances list.
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

-- | Distributions accompained by related addresses set (what to
-- distribute and how).
type AddrDistribution = ([Address], StakeDistribution)

-- | Hardcoded genesis data to generate utxo from.
data GenesisCoreData = UnsafeGenesisCoreData
    { gcdAddrDistribution      :: !([AddrDistribution])
      -- ^ Address distribution. Determines utxo without boot
      -- stakeholders distribution (addresses and coins).
    , gcdBootstrapStakeholders :: !(HashSet StakeholderId)
      -- ^ Bootstrap era stakeholders.
    } deriving (Show, Eq)

-- | Safe constructor for 'GenesisCoreData'. Throws error if something
-- goes wrong.
mkGenesisCoreData ::
       [AddrDistribution]
    -> HashSet StakeholderId
    -> Either String GenesisCoreData
mkGenesisCoreData distribution bootStakeholders = do
    -- TODO CSL-1205 add checks
    -- 1. Every set of address matches by the size to distribution size
    -- 2. If using CustomStakes, lengths match
    -- 3. (?) Addresses in (map fst) are unique
    pure $ UnsafeGenesisCoreData distribution bootStakeholders
