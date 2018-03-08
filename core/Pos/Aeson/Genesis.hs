-- | Aeson instances for GenesisSpec and related datatypes.

module Pos.Aeson.Genesis
       (
        -- * ToJSONKey RedeemPublicKey
        -- * FromJSONKey RedeemPublicKey
        -- * ToJSON
        -- ** GenesisAvvmBalances
        -- ** GenesisWStakeholders
        -- ** GenesisNonAvvmBalances
        -- ** VssCertificatesMap
        -- ** GenesisVssCertificatesMap
        -- ** GenesisDelegation
        -- ** FakeAvvmOptions
        -- ** TestnetBalanceOptions
        -- ** GenesisInitializer
        -- ** ProtocolConstants
        -- ** GenesisSpec
        -- * FromJSON
        -- ** GenesisAvvmBalances
        -- ** GenesisWStakeholders
        -- ** GenesisNonAvvmBalances
        -- ** VssCertificatesMap
        -- ** GenesisVssCertificatesMap
        -- ** GenesisDelegation
        -- ** FakeAvvmOptions
        -- ** TestnetBalanceOptions
        -- ** GenesisInitializer
        -- ** ProtocolConstants
        -- ** GenesisSpec
       ) where

import           Universum

import           Control.Lens (_Left)
import           Data.Aeson (FromJSON (..), FromJSONKey (..), FromJSONKeyFunction (..), ToJSON (..),
                             ToJSONKey (..), ToJSONKeyFunction (..))
import           Data.Aeson.Encoding (text)
import           Data.Aeson.TH (deriveJSON)
import qualified Data.HashMap.Strict as HM
import           Formatting (sformat)
import           Serokell.Aeson.Options (defaultOptions)

import           Pos.Aeson.Core ()
import           Pos.Aeson.Crypto ()
import           Pos.Binary.Core.Address ()
import           Pos.Core.Common (Address, Coin, StakeholderId, unsafeGetCoin)
import           Pos.Core.Delegation (ProxySKHeavy)
import           Pos.Core.Genesis.Helpers (convertNonAvvmDataToBalances, recreateGenesisDelegation)
import           Pos.Core.Genesis.Types (FakeAvvmOptions, GenesisAvvmBalances (..),
                                         GenesisDelegation, GenesisInitializer,
                                         GenesisNonAvvmBalances (..), GenesisSpec,
                                         GenesisVssCertificatesMap (..), GenesisWStakeholders (..),
                                         ProtocolConstants, TestnetBalanceOptions,
                                         unGenesisDelegation)
import           Pos.Core.Ssc (VssCertificatesMap (..), getVssCertificatesMap)
import           Pos.Crypto (RedeemPublicKey, fromAvvmPk, redeemPkB64UrlF)
import           Pos.Util.Util (toAesonError)

instance ToJSONKey RedeemPublicKey where
    toJSONKey = ToJSONKeyText render (text . render)
      where
        render = sformat redeemPkB64UrlF

instance FromJSONKey RedeemPublicKey where
    fromJSONKey = FromJSONKeyTextParser (toAesonError . over _Left pretty . fromAvvmPk)
    fromJSONKeyList = FromJSONKeyTextParser (toAesonError . bimap pretty pure . fromAvvmPk)

deriving instance ToJSON GenesisAvvmBalances
deriving instance FromJSON GenesisAvvmBalances
deriving instance ToJSON GenesisWStakeholders
deriving instance FromJSON GenesisWStakeholders

instance ToJSON GenesisNonAvvmBalances where
    toJSON = toJSON . convert . getGenesisNonAvvmBalances
      where
        convert :: HashMap Address Coin -> HashMap Text Integer
        convert = HM.fromList . map f . HM.toList
        f :: (Address, Coin) -> (Text, Integer)
        f = bimap pretty (toInteger . unsafeGetCoin)

instance FromJSON GenesisNonAvvmBalances where
    parseJSON = toAesonError . convertNonAvvmDataToBalances <=< parseJSON

instance ToJSON VssCertificatesMap where
    toJSON = toJSON . getVssCertificatesMap

instance FromJSON VssCertificatesMap where
    parseJSON val = UncheckedVssCertificatesMap <$> parseJSON val

instance ToJSON GenesisVssCertificatesMap where
    toJSON = toJSON . getGenesisVssCertificatesMap

instance FromJSON GenesisVssCertificatesMap where
    parseJSON val = GenesisVssCertificatesMap <$> parseJSON val

instance ToJSON GenesisDelegation where
    toJSON = toJSON . unGenesisDelegation

instance FromJSON GenesisDelegation where
    parseJSON = parseJSON >=> \v -> do
        (elems :: HashMap StakeholderId ProxySKHeavy) <- mapM parseJSON v
        toAesonError $ recreateGenesisDelegation elems

deriveJSON defaultOptions ''FakeAvvmOptions
deriveJSON defaultOptions ''TestnetBalanceOptions
deriveJSON defaultOptions ''GenesisInitializer
deriveJSON defaultOptions ''ProtocolConstants
deriveJSON defaultOptions ''GenesisSpec
