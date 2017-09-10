-- | Types related to genesis core data.

module Pos.Core.Genesis.Types
       ( BalanceDistribution (..)
       , getDistributionSize
       , getTotalBalance
       , safeExpBalances

       , AddrDistribution
       , GenesisWStakeholders (..)
       , GenesisDelegation (..)
       , noGenesisDelegation
       , mkGenesisDelegation
       , GenesisSpec (..)
       , TestBalanceOptions (..)
       , FakeAvvmOptions (..)
       , GenesisCoreData (..)
       , bootDustThreshold
       , mkGenesisCoreData
       ) where

import           Universum

import           Control.Lens         (at)
import           Control.Monad.Except (MonadError (throwError))
import qualified Data.HashMap.Strict  as HM
import qualified Data.Map.Lazy        as M
import qualified Data.Text.Buildable  as Buildable
import           Formatting           (bprint, sformat, (%))
import           Serokell.Util        (allDistinct, mapJson)
import qualified Serokell.Util.Base16 as B16
import           Text.JSON.Canonical  (Int54, JSValue (..), ToJSON (..), ToObjectKey (..))

import           Pos.Binary.Class     (serialize')
import           Pos.Core.Address     (addressHash, isBootstrapEraDistrAddress)
import           Pos.Core.Coin        (coinToInteger, sumCoins, unsafeGetCoin,
                                       unsafeIntegerToCoin)
import           Pos.Core.Types       (Address, Coin, EpochIndex, ProxySKHeavy,
                                       StakeholderId, mkCoin)
import           Pos.Crypto           (ProxyCert, ProxySecretKey (..), PublicKey,
                                       fullPublicKeyHexF, hashHexF, isSelfSignedPsk)

-- | Balances distribution in genesis block.
data BalanceDistribution
    -- | FlatBalances is a flat distribution, i. e. each node has the
    -- same amount of coins.
    = FlatBalances !Word     -- ^ Number of stakeholders
                   !Coin     -- ^ Total number of coins
    -- | Rich/poor distribution, for testnet mostly.
    | RichPoorBalances
        { sdRichmen     :: !Word
        , sdRichBalance :: !Coin
        , sdPoor        :: !Word
        , sdPoorBalance :: !Coin
        }
    -- | First three nodes get 0.875% of balance.
    | ExponentialBalances !Word -- ^ Numbers of participants
                          !Coin -- ^ Minimal coin
    -- | Custom balances list.
    | CustomBalances [Coin]
    deriving (Show, Eq, Generic)

-- | Get the amount of stakeholders in a distribution.
getDistributionSize :: BalanceDistribution -> Word
getDistributionSize (FlatBalances n _)         = n
getDistributionSize (RichPoorBalances a _ b _) = a + b
getDistributionSize (ExponentialBalances n _)  = n
getDistributionSize (CustomBalances cs)        = fromIntegral (length cs)

-- | Get total amount of balance in a distribution.
getTotalBalance :: BalanceDistribution -> Coin
getTotalBalance (FlatBalances _ st) = st
getTotalBalance RichPoorBalances {..} = unsafeIntegerToCoin $
    coinToInteger sdRichBalance * fromIntegral sdRichmen +
    coinToInteger sdPoorBalance * fromIntegral sdPoor
getTotalBalance (ExponentialBalances n (fromIntegral . unsafeGetCoin -> mc)) =
    mkCoin $ sum $ take (fromIntegral n) $ iterate (*2) mc
getTotalBalance (CustomBalances balances) =
    unsafeIntegerToCoin $ sumCoins balances

-- | Generates exponential balances that will be valid in boot era prior
-- to number of participants.
--
-- Exponential balances have the form @map (*b) [2^0, 2^1, 2^2, ...]@,
-- where @b@ is the last argument of @ExponentialBalances@. It means
-- that when distribution balances are created, @b@ is their common
-- divisor, so weights are @[2^0, 2^1, ..]@. We also require that no
-- genesis balance is lower than sum of weights. So if balances list has
-- length @k@ we have weights sum @2^{k+1}-1@. That's why the lowest
-- coin is taken to be @2^{k+1}@.
safeExpBalances :: (Integral a) => a -> BalanceDistribution
safeExpBalances n =
    -- This function should be used on start only so if this
    -- `unsafeIntegerToCoin` fails it means we've misconfigured
    -- something and it's easy to find/fix.
    ExponentialBalances (fromIntegral n) (unsafeIntegerToCoin $ (2::Integer) ^ n)

-- | Distributions accompained by related addresses set (what to
-- distribute and how).
type AddrDistribution = ([Address], BalanceDistribution)

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

data TestBalanceOptions = TestBalanceOptions
    { tsoPattern      :: FilePath
    , tsoPoors        :: Word
    , tsoRichmen      :: Word
    , tsoRichmenShare :: Double
    , tsoTotalBalance :: Word64
    } deriving (Show)

data FakeAvvmOptions = FakeAvvmOptions
    { faoCount      :: Word
    , faoOneBalance :: Word64
    } deriving (Show)

--- | Generated genesis data, generated from genesis.json to generate utxo from.
data GenesisCoreData = UnsafeGenesisCoreData
    { gcdAddrDistribution      :: ![AddrDistribution]
      -- ^ Address distribution. Determines utxo without boot
      -- stakeholders distribution (addresses and coins).
    , gcdBootstrapStakeholders :: !GenesisWStakeholders
      -- ^ Bootstrap era stakeholders, values are weights.
    , gcdHeavyDelegation       :: !GenesisDelegation
      -- ^ Genesis state of heavyweight delegation.
    } deriving (Show, Eq, Generic)


-- | Hardcoded genesis data to generate utxo from.
data GenesisSpec = GenesisSpec
    { gsAvvmData        :: !GenesisCoreData
    -- ^ Genesis data describes avvm.
    -- Only this field must be presented in mainnet.

    -- Fields below describe parameters to generate utxo for testnet.
    , gsTestBalance     :: !(Maybe TestBalanceOptions)
    , gsFakeAvvmBalance :: !(Maybe FakeAvvmOptions)
      -- ^ Explicit bootstrap era stakeholders, list of addresses with
      -- weights (@[(A, 5), (B, 2), (C, 3)]@). Setting this
      -- overrides default settings for boot stakeholders (e.g. rich
      -- in testnet stakes).
    , gsSeed            :: !(Maybe Integer)
      -- ^ Seed to use (when no seed is provided, a secure random generator
      -- is used)
    } deriving (Show, Generic)

----------------------------------------------------------------------------
-- ToJSON/FromJSON instances
----------------------------------------------------------------------------

wordToJSON :: Word -> JSValue
wordToJSON = JSNum . fromIntegral

word64ToJSON :: Word64 -> JSValue
word64ToJSON = JSString . show

integerToJSON :: Word64 -> JSValue
integerToJSON = JSString . show

coinToJSON :: Coin -> JSValue
coinToJSON = word64ToJSON . unsafeGetCoin

doubleToJSON :: Double -> JSValue
doubleToJSON = JSString . show

instance Monad m => ToJSON m Word16 where
    toJSON = pure . JSString . show

instance Monad m => ToJSON m Coin where
    toJSON = pure . coinToJSON

instance Monad m => ToJSON m Address where
    toJSON = pure . JSString . show . B16.encode . serialize'

instance Monad m => ToJSON m PublicKey where
    toJSON = pure . JSString . show . sformat fullPublicKeyHexF

instance Monad m => ToJSON m EpochIndex where
    toJSON = pure . word64ToJSON . fromIntegral

instance (Monad m, Typeable w) => ToJSON m (ProxyCert w) where
    toJSON = pure . JSString . show . B16.encode . serialize'

instance (Monad m, ToJSON m w, Typeable w) => ToJSON m (ProxySecretKey w) where
    toJSON ProxySecretKey{..} = fmap JSObject $ sequence $
        [ ("omega",)      <$> toJSON pskOmega
        , ("issuerPk",)   <$> toJSON pskIssuerPk
        , ("delegatePk",) <$> toJSON pskDelegatePk
        , ("cert",)       <$> toJSON pskCert
        ]

instance Monad m => ToJSON m BalanceDistribution where
    toJSON (FlatBalances n coin) = pure $
        JSObject [("FlatBalances",
            JSObject [ ("n", wordToJSON n)
                     , ("coin", coinToJSON coin)
                     ]
                 )]
    toJSON RichPoorBalances{..} = pure $
        JSObject [("RichPoorBalances",
            JSObject [ ("richmen", wordToJSON sdRichmen)
                     , ("richBalance", coinToJSON sdRichBalance)
                     , ("poor", wordToJSON sdPoor)
                     , ("poorBalance", coinToJSON sdPoorBalance)
                     ]
                 )]
    toJSON (ExponentialBalances n minCoin) = pure $
        JSObject [("ExponentialBalances",
            JSObject [ ("n", wordToJSON n)
                     , ("minCoin", coinToJSON minCoin)
                     ]
                 )]
    toJSON (CustomBalances coins) = do
        coinsList <- toJSON coins
        pure $
            JSObject [("CustomBalances",
                JSObject [("coins", coinsList)]
                     )]

instance Monad m => ToJSON m AddrDistribution where
    toJSON (addresses, balDistr) = do
        addressesField <- ("addresses",) <$> toJSON addresses
        balDistrField <- ("balanceDistribution",) <$> toJSON balDistr
        pure $ JSObject [addressesField, balDistrField]

instance Monad m => ToObjectKey m StakeholderId where
    toObjectKey = pure . show . sformat hashHexF

instance Monad m => ToJSON m GenesisWStakeholders where
    toJSON (GenesisWStakeholders stks) = toJSON stks

instance Monad m => ToJSON m GenesisDelegation where
    toJSON (UnsafeGenesisDelegation hm) = toJSON $ M.fromList $ HM.toList hm

instance Monad m => ToJSON m GenesisCoreData where
    toJSON UnsafeGenesisCoreData {..} = fmap JSObject $ sequence $
        [ ("addrDistribution",) <$> toJSON gcdAddrDistribution
        , ("bootStakeholders",) <$> toJSON gcdBootstrapStakeholders
        , ("heavyDelegation",)  <$> toJSON gcdHeavyDelegation
        ]

instance Monad m => ToJSON m TestBalanceOptions where
    toJSON TestBalanceOptions{..} = pure $ JSObject
        [ ("pattern", JSString tsoPattern)
        , ("poors", wordToJSON tsoPoors)
        , ("richmen", wordToJSON tsoRichmen)
        , ("richmenShare", doubleToJSON tsoRichmenShare)
        , ("totalBalance", word64ToJSON tsoTotalBalance)
        ]

instance Monad m => ToJSON m FakeAvvmOptions where
    toJSON FakeAvvmOptions{..} = pure $ JSObject $
        [ ("count", wordToJSON faoCount)
        , ("oneBalance", word64ToJSON faoOneBalance)
        ]

instance Monad m => ToJSON m GenesisSpec where
    toJSON GenesisSpec {..} = do
        avvmData <- ("avvmData",) <$> toJSON gsAvvmData
        optionalTestnetFields <- fmap catMaybes $ sequence [
            ("testBalance",) <<$>> traverse toJSON gsTestBalance
          , ("fakeAvvmBalance",) <<$>> traverse toJSON gsFakeAvvmBalance
          , ("seed",) . JSNum . fromIntegral <<$>> pure gsSeed
          ]
        pure $ JSObject $ avvmData : optionalTestnetFields

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
mkGenesisCoreData
    :: [AddrDistribution]
    -> Map StakeholderId Word16
    -> GenesisDelegation
    -> Either String GenesisCoreData
mkGenesisCoreData distribution bootStakeholders delega = do
    for_ distribution $ \(addrs, distr) -> do
        -- Every set of addresses should match the stakeholders count
        unless (fromIntegral (length addrs) == getDistributionSize distr) $
            Left "mkGenesisCoreData: addressCount != stakeholdersCount \
                 \for some set of addresses"
        -- Addresses in each list are distinct (except for CustomBalances)
        let isCustom = case distr of
                CustomBalances{} -> True
                _                -> False
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
