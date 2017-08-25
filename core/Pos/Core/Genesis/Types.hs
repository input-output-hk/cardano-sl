-- | Types related to genesis core data.

module Pos.Core.Genesis.Types
       ( StakeDistribution (..)
       , getDistributionSize
       , getTotalStake
       , safeExpStakes

       , AddrDistribution
       , GenesisWStakeholders (..)
       , GenesisCoreData (..)
       , bootDustThreshold
       , mkGenesisCoreData
       ) where

import           Universum

import qualified Data.HashMap.Strict as HM
import qualified Data.Text.Buildable as Buildable
import           Formatting          (bprint, sformat, (%))
import           Serokell.Util       (allDistinct, listJson, pairF)

import           Pos.Core.Coin       (coinToInteger, sumCoins, unsafeGetCoin,
                                      unsafeIntegerToCoin)
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
    -- | Rich/poor distribution, for testnet mostly.
    | RichPoorStakes
        { sdRichmen   :: !Word
        , sdRichStake :: !Coin
        , sdPoor      :: !Word
        , sdPoorStake :: !Coin
        }
    -- | First three nodes get 0.875% of balance.
    | ExponentialStakes !Word -- ^ Numbers of participants
                        !Coin -- ^ Minimal coin
    -- | Custom balances list.
    | CustomStakes [Coin]
    deriving (Show, Eq, Generic)

-- | Get the amount of stakeholders in a distribution.
getDistributionSize :: StakeDistribution -> Word
getDistributionSize (FlatStakes n _)         = n
getDistributionSize (RichPoorStakes a _ b _) = a + b
getDistributionSize (ExponentialStakes n _)  = n
getDistributionSize (CustomStakes cs)        = fromIntegral (length cs)

-- | Get total amount of stake in a distribution.
getTotalStake :: StakeDistribution -> Coin
getTotalStake (FlatStakes _ st) = st
getTotalStake RichPoorStakes {..} = unsafeIntegerToCoin $
    coinToInteger sdRichStake * fromIntegral sdRichmen +
    coinToInteger sdPoorStake * fromIntegral sdPoor
getTotalStake (ExponentialStakes n (fromIntegral . unsafeGetCoin -> mc)) =
    mkCoin $ sum $ take (fromIntegral n) $ iterate (*2) mc
getTotalStake (CustomStakes balances) =
    unsafeIntegerToCoin $ sumCoins balances

-- | Generates exponential stakes that will be valid in boot era prior
-- to number of participants.
--
-- Exponential stakes have the form @map (*b) [2^0, 2^1, 2^2, ...]@,
-- where @b@ is the last argument of @ExponentialStakes@. It means
-- that when distribution stakes are created, @b@ is their common
-- divisor, so weights are @[2^0, 2^1, ..]@. We also require that no
-- genesis balance is lower than sum of weights. So if stakes list has
-- length @k@ we have weights sum @2^{k+1}-1@. That's why the lowest
-- coin is taken to be @2^{k+1}@.
safeExpStakes :: (Integral a) => a -> StakeDistribution
safeExpStakes n =
    -- This function should be used on start only so if this
    -- `unsafeIntegerToCoin` fails it means we've misconfigured
    -- something and it's easy to find/fix.
    ExponentialStakes (fromIntegral n) (unsafeIntegerToCoin $ (2::Integer) ^ n)

-- | Distributions accompained by related addresses set (what to
-- distribute and how).
type AddrDistribution = ([Address], StakeDistribution)

-- | Wrapper around weighted stakeholders map to be used in genesis
-- core data.
newtype GenesisWStakeholders = GenesisWStakeholders
    { getGenesisWStakeholders :: HashMap StakeholderId Word16
    } deriving (Show, Eq)

instance Buildable GenesisWStakeholders where
    build (GenesisWStakeholders m) =
        bprint ("GenesisWStakeholders: "%listJson)
               (map (sformat pairF) $ HM.toList m)

-- | Hardcoded genesis data to generate utxo from.
data GenesisCoreData = UnsafeGenesisCoreData
    { gcdAddrDistribution      :: ![AddrDistribution]
      -- ^ Address distribution. Determines utxo without boot
      -- stakeholders distribution (addresses and coins).
    , gcdBootstrapStakeholders :: !GenesisWStakeholders
      -- ^ Bootstrap era stakeholders, values are weights.
    } deriving (Show, Eq, Generic)

-- | Calculates a minimum amount of coins user can set as an output in
-- boot era.
bootDustThreshold :: GenesisWStakeholders -> Coin
bootDustThreshold (GenesisWStakeholders bootHolders) =
    -- it's safe to use it here because weights are word16 and should
    -- be really low in production, so this sum is not going to be
    -- even more than 10-15 coins.
    unsafeIntegerToCoin . sum $ map fromIntegral $ HM.elems bootHolders

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
