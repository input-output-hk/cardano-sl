-- | Types related to genesis core data.

module Pos.Core.Genesis.Types
       ( StakeDistribution (..)
       , getDistributionSize
       , getTotalStake

       , AddrDistribution
       , GenesisWStakeholders (..)
       , GenesisCoreData (..)
       , bootRelatedDistr
       , mkGenesisCoreData
       ) where

import           Universum

import qualified Data.HashMap.Strict as HM
import           Serokell.Util       (allDistinct)

import           Pos.Core.Coin       (coinToInteger, sumCoins, unsafeIntegerToCoin)
import           Pos.Core.Types      (Address, Coin, StakeholderId, mkCoin)

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
    | ExponentialStakes !Word
    -- | Custom balances list.
    | CustomStakes [Coin]
    deriving (Show, Eq, Generic)

-- | Get the amount of stakeholders in a distribution.
getDistributionSize :: StakeDistribution -> Word
getDistributionSize (FlatStakes n _)         = n
getDistributionSize (BitcoinStakes n _)      = n
getDistributionSize (RichPoorStakes a _ b _) = a + b
getDistributionSize (ExponentialStakes n)    = n
getDistributionSize (CustomStakes cs)        = fromIntegral (length cs)

-- | Get total amount of stake in a distribution.
getTotalStake :: StakeDistribution -> Coin
getTotalStake (FlatStakes _ st) = st
getTotalStake (BitcoinStakes _ st) = st
getTotalStake RichPoorStakes {..} = unsafeIntegerToCoin $
    coinToInteger sdRichStake * fromIntegral sdRichmen +
    coinToInteger sdPoorStake * fromIntegral sdPoor
getTotalStake (ExponentialStakes n) =
    mkCoin $ sum $ map (2^) [0 .. n - 1]
getTotalStake (CustomStakes balances) =
    unsafeIntegerToCoin $ sumCoins balances

-- | Distributions accompained by related addresses set (what to
-- distribute and how).
type AddrDistribution = ([Address], StakeDistribution)

-- | Wrapper around weighted stakeholders map to be used in genesis
-- core data.
newtype GenesisWStakeholders = GenesisWStakeholders
    { getGenesisWStakeholders :: HashMap StakeholderId Word16
    } deriving (Show, Eq)

-- | Hardcoded genesis data to generate utxo from.
data GenesisCoreData = UnsafeGenesisCoreData
    { gcdAddrDistribution      :: ![AddrDistribution]
      -- ^ Address distribution. Determines utxo without boot
      -- stakeholders distribution (addresses and coins).
    , gcdBootstrapStakeholders :: !GenesisWStakeholders
      -- ^ Bootstrap era stakeholders, values are weights.
    } deriving (Show, Eq, Generic)


-- | Checks whether txOutDistribution matches the set of weighted boot
-- stakeholders. Notice: it doesn't use actual txdistr type because
-- it's defined in txp module above.
-- TODO it doesn't count for weights.
-- TODO it doesn't check that distribution of coins is not shifted to single user.
bootRelatedDistr :: GenesisWStakeholders -> [(StakeholderId, Coin)] -> Bool
bootRelatedDistr (GenesisWStakeholders bootHolders) txOutDistr =
    -- All addresses in txDistr are from bootHolders
    all inBoot addrs &&
    -- Every boot stakeholder is mentioned in txOutDistr
    all (`elem` addrs) (HM.keys bootHolders)
  where
    addrs = map fst txOutDistr
    inBoot s = s `HM.member` bootHolders

-- | Safe constructor for 'GenesisCoreData'. Throws error if something
-- goes wrong.
mkGenesisCoreData ::
       [AddrDistribution]
    -> HashMap StakeholderId Word16
    -> Either String GenesisCoreData
mkGenesisCoreData distribution bootStakeholders = do
    -- Every set of addresses should match the stakeholders count
    for_ distribution $ \(addrs, distr) ->
        unless (fromIntegral (length addrs) == getDistributionSize distr) $
            Left "mkGenesisCoreData: addressCount != stakeholdersCount \
                 \for some set of addresses"
    -- Addresses in each list are distinct (except for CustomStakes)
    for_ distribution $ \(addrs, distr) -> do
        let isCustom = case distr of
                CustomStakes{} -> True
                _              -> False
        unless (isCustom || allDistinct addrs) $
            Left "mkGenesisCoreData: addresses in some list aren't distinct"
    -- No address belongs to more than one distribution
    let addrList = concatMap (ordNub . fst) distribution
    unless (allDistinct addrList) $
        Left "mkGenesisCoreData: some address belongs to more than one distr"
    -- All checks passed
    pure $ UnsafeGenesisCoreData distribution (GenesisWStakeholders bootStakeholders)
