-- | Types related to genesis core data.

module Pos.Core.Genesis.Types
       ( StakeDistribution (..)
       , getDistributionSize
       , getTotalStake
       , safeExpStakes

       , AddrDistribution
       , GenesisWStakeholders (..)
       , GenesisDelegation (..)
       , noGenesisDelegation
       , mkGenesisDelegation
       , GenesisCoreData (..)
       , bootDustThreshold
       , mkGenesisCoreData
       ) where

import           Universum

import           Control.Lens         (at)
import           Control.Monad.Except (MonadError (throwError))
import qualified Data.HashMap.Strict  as HM
import qualified Data.Text.Buildable  as Buildable
import           Formatting           (bprint, (%))
import           Serokell.Util        (allDistinct, mapJson)

import           Pos.Core.Address     (addressHash, isBootstrapEraDistrAddress)
import           Pos.Core.Coin        (coinToInteger, sumCoins, unsafeGetCoin,
                                       unsafeIntegerToCoin)
import           Pos.Core.Types       (Address, Coin, ProxySKHeavy, StakeholderId, mkCoin)
import           Pos.Crypto           (ProxySecretKey (..), isSelfSignedPsk)

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
    { getGenesisWStakeholders :: Map StakeholderId Word16
    } deriving (Show, Eq)

instance Buildable GenesisWStakeholders where
    build (GenesisWStakeholders m) =
        bprint ("GenesisWStakeholders: "%mapJson) m

-- | This type contains genesis state of heavyweight delegation. It
-- wraps a map where keys are issuers (i. e. stakeholders who
-- delegated) and values are proxy signing keys. There are some invariants:
-- 1. In each pair delegate must differ from issuer, i. e. no revocations.
-- 2. PSKs must be consistent with keys in the map, i. e. issuer's ID must be
--    equal to the key in the map.
-- 3. Delegates can't be issuers, i. e. transitive delegation is not supported.
--    It's not needed in genesis, it can always be reduced.
newtype GenesisDelegation = UnsafeGenesisDelegation
    { unGenesisDelegation :: HashMap StakeholderId ProxySKHeavy
    } deriving (Show, Eq)

-- | Empty 'GenesisDelegation'.
noGenesisDelegation :: GenesisDelegation
noGenesisDelegation = UnsafeGenesisDelegation mempty

-- | Safe constructor of 'GenesisDelegation'.
mkGenesisDelegation ::
       MonadError Text m
    => [ProxySKHeavy]
    -> m GenesisDelegation
mkGenesisDelegation psks = do
    unless (allDistinct $ pskIssuerPk <$> psks) $
        throwError "all issuers must be distinct"
    when (any isSelfSignedPsk psks) $
        throwError "there is a self-signed (revocation) psk"
    let resPairs =
            psks <&> \psk@ProxySecretKey {..} -> (addressHash pskIssuerPk, psk)
    let resMap = HM.fromList resPairs
    let isIssuer ProxySecretKey {..} =
            isJust $ resMap ^. at (addressHash pskDelegatePk)
    when (any isIssuer psks) $
        throwError "one of the delegates is also an issuer, don't do it"
    return $ UnsafeGenesisDelegation resMap

-- | Hardcoded genesis data to generate utxo from.
data GenesisCoreData = UnsafeGenesisCoreData
    { gcdAddrDistribution      :: ![AddrDistribution]
      -- ^ Address distribution. Determines utxo without boot
      -- stakeholders distribution (addresses and coins).
    , gcdBootstrapStakeholders :: !GenesisWStakeholders
      -- ^ Bootstrap era stakeholders, values are weights.
    , gcdHeavyDelegation       :: !GenesisDelegation
      -- ^ Genesis state of heavyweight delegation.
    } deriving (Show, Eq, Generic)

-- | Calculates a minimum amount of coins user can set as an output in
-- boot era.
bootDustThreshold :: GenesisWStakeholders -> Coin
bootDustThreshold (GenesisWStakeholders bootHolders) =
    -- it's safe to use it here because weights are word16 and should
    -- be really low in production, so this sum is not going to be
    -- even more than 10-15 coins.
    unsafeIntegerToCoin . sum $ map fromIntegral $ toList bootHolders

-- | Safe constructor for 'GenesisCoreData'. Throws error if something
-- goes wrong.
mkGenesisCoreData ::
       [AddrDistribution]
    -> Map StakeholderId Word16
    -> GenesisDelegation
    -> Either String GenesisCoreData
mkGenesisCoreData distribution bootStakeholders delega = do
    for_ distribution $ \(addrs, distr) -> do
        -- Every set of addresses should match the stakeholders count
        unless (fromIntegral (length addrs) == getDistributionSize distr) $
            Left "mkGenesisCoreData: addressCount != stakeholdersCount \
                 \for some set of addresses"
        -- Addresses in each list are distinct (except for CustomStakes)
        let isCustom = case distr of
                CustomStakes{} -> True
                _              -> False
        unless (isCustom || allDistinct addrs) $
            Left "mkGenesisCoreData: addresses in some list aren't distinct"
        unless (all isBootstrapEraDistrAddress addrs) $
            Left $ "mkGenesisCoreData: there is an address with stake " <>
                   "distribution different from BootstrapEraDistr"

    -- No address belongs to more than one distribution
    let addrList = concatMap (ordNub . fst) distribution
    unless (allDistinct addrList) $
        Left "mkGenesisCoreData: some address belongs to more than one distr"
    -- All checks passed
    pure
        UnsafeGenesisCoreData
        { gcdAddrDistribution = distribution
        , gcdBootstrapStakeholders = GenesisWStakeholders bootStakeholders
        , gcdHeavyDelegation = delega
        }
