-- |

module Pos.Core.Genesis.JSON where

import           Universum

import qualified Data.HashMap.Strict    as HM
import qualified Data.Map.Lazy          as M
import           Formatting             (sformat)
import qualified Serokell.Util.Base16   as B16
import           Text.JSON.Canonical    (JSValue (..), ToJSON (..), ToObjectKey (..))

import           Pos.Binary.Class       (serialize')
import           Pos.Core.Genesis.Types (AddrDistribution, BalanceDistribution (..),
                                         FakeAvvmOptions (..), GenesisAddrDistr (..),
                                         GenesisDelegation (..), GenesisInitializer (..),
                                         GenesisSpec (..), GenesisWStakeholders (..),
                                         TestBalanceOptions (..),
                                         TestnetDistribution (..))
import           Pos.Core.Timestamp     (Timestamp (..))
import           Pos.Core.Types         (Address, Coin, EpochIndex, StakeholderId,
                                         unsafeGetCoin)
import           Pos.Crypto             (ProxyCert, ProxySecretKey (..), PublicKey,
                                         fullPublicKeyHexF, hashHexF)

----------------------------------------------------------------------------
-- ToJSON/FromJSON instances
----------------------------------------------------------------------------

wordToJSON :: Word -> JSValue
wordToJSON = JSNum . fromIntegral

word64ToJSON :: Word64 -> JSValue
word64ToJSON = JSString . show

integerToJSON :: Integer -> JSValue
integerToJSON = JSString . show

doubleToJSON :: Double -> JSValue
doubleToJSON = JSString . show

coinToJSON :: Coin -> JSValue
coinToJSON = word64ToJSON . unsafeGetCoin

instance Monad m => ToJSON m Word16 where
    toJSON = pure . JSString . show

instance Monad m => ToJSON m Integer where
    toJSON = pure . integerToJSON

instance Monad m => ToJSON m Coin where
    toJSON = pure . coinToJSON

instance Monad m => ToJSON m Timestamp where
    toJSON = pure . word64ToJSON . fromIntegral . getTimestamp

instance Monad m => ToObjectKey m StakeholderId where
    toObjectKey = pure . show . sformat hashHexF

instance Monad m => ToJSON m Address where
    toJSON = pure . JSString . show . B16.encode . serialize'

instance Monad m => ToObjectKey m Address where
    toObjectKey = pure . show . B16.encode . serialize'

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

instance Monad m => ToJSON m GenesisWStakeholders where
    toJSON (GenesisWStakeholders stks) = toJSON stks

instance Monad m => ToJSON m TestBalanceOptions where
    toJSON TestBalanceOptions{..} = pure $ JSObject
        [ ("poors", wordToJSON tsoPoors)
        , ("richmen", wordToJSON tsoRichmen)
        , ("richmenShare", doubleToJSON tsoRichmenShare)
        , ("totalBalance", word64ToJSON tsoTotalBalance)
        ]

instance Monad m => ToJSON m FakeAvvmOptions where
    toJSON FakeAvvmOptions{..} = pure $ JSObject $
        [ ("count", wordToJSON faoCount)
        , ("oneBalance", word64ToJSON faoOneBalance)
        ]

instance Monad m => ToJSON m GenesisAddrDistr where
    toJSON (GenesisAddrDistr hm) = toJSON $ M.fromList $ HM.toList hm

instance Monad m => ToJSON m GenesisDelegation where
    toJSON (UnsafeGenesisDelegation hm) = toJSON $ M.fromList $ HM.toList hm

instance Monad m => ToJSON m TestnetDistribution where
    toJSON TestnetBalanceStakeDistr = pure $ JSObject [("BalanceStakeDistr", JSObject [])]
    toJSON TestnetRichmenStakeDistr = pure $ JSObject [("RichmenStakeDistr", JSObject [])]
    toJSON TestnetCustomStakeDistr{..} = JSObject . one . ("CustomStakeDistr",) . JSObject <$>
        sequence [
            ("bootStakeholders",) <$> toJSON tcsdBootStakeholders
          ]

instance Monad m => ToJSON m GenesisInitializer where
    toJSON TestnetInitializer{..} = JSObject . one . ("Testnet",) . JSObject <$>
        sequence [
            ("balances",) <$> toJSON tiTestBalance
          , ("fakeAvvm",) <$> toJSON tiFakeAvvmBalance
          , ("distr",)    <$> toJSON tiDistribution
          , ("seed",)     <$> toJSON tiSeed
          ]

    toJSON MainnetInitializer{..} = JSObject . one . ("Mainnet",) . JSObject <$>
        sequence [
            ("startTime",)         <$> toJSON miStartTime
          , ("bootStakeholders", ) <$> toJSON miBootStakeholders
          ]

instance Monad m => ToJSON m GenesisSpec where
    toJSON UnsafeGenesisSpec {..} = JSObject <$> sequence [
        ("avvmDistr",)        <$> toJSON gsAvvmDistr
      , ("heavyDelegation",) <$> toJSON gsHeavyDelegation
      , ("initializer",)      <$> toJSON gsInitializer
      ]

