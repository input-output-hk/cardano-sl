-- | Aeson instances for GenesisSpec and related datatypes.

module Pos.Aeson.Genesis
       ( fromAvvmPk
       ) where

import           Universum

import           Data.Aeson              (FromJSON (..), FromJSONKey (..),
                                          FromJSONKeyFunction (..))
import           Data.Aeson.TH           (deriveFromJSON)
import           Serokell.Aeson.Options  (defaultOptions)

import           Pos.Aeson.Core          ()
import           Pos.Aeson.Crypto        ()
import           Pos.Binary.Core.Address ()
import           Pos.Core.Genesis.Types  (FakeAvvmOptions, GenesisAvvmBalances (..),
                                          GenesisDelegation, GenesisInitializer,
                                          GenesisNonAvvmBalances, GenesisSpec,
                                          GenesisVssCertificatesMap (..),
                                          GenesisWStakeholders (..), ProtocolConstants,
                                          TestnetBalanceOptions, TestnetDistribution,
                                          convertNonAvvmDataToBalances,
                                          mkGenesisDelegation)
import           Pos.Core.Types          (ProxySKHeavy, StakeholderId)
import           Pos.Core.Vss            (validateVssCertificatesMap)
import           Pos.Crypto              (RedeemPublicKey, fromAvvmPk)
import           Pos.Util.Util           (eitherToFail)

instance FromJSONKey RedeemPublicKey where
    fromJSONKey = FromJSONKeyTextParser fromAvvmPk
    fromJSONKeyList = FromJSONKeyTextParser (fmap pure . fromAvvmPk)

deriving instance FromJSON GenesisAvvmBalances
deriving instance FromJSON GenesisWStakeholders

instance FromJSON GenesisNonAvvmBalances where
    parseJSON = convertNonAvvmDataToBalances <=< parseJSON

instance FromJSON GenesisVssCertificatesMap where
    parseJSON = parseJSON >=> \mE ->
        eitherToFail $ GenesisVssCertificatesMap <$> validateVssCertificatesMap mE

instance FromJSON GenesisDelegation where
    parseJSON = parseJSON >=> \v -> do
        (elems :: HashMap StakeholderId ProxySKHeavy) <- mapM parseJSON v
        eitherToFail $ mkGenesisDelegation elems

deriveFromJSON defaultOptions ''TestnetDistribution
deriveFromJSON defaultOptions ''FakeAvvmOptions
deriveFromJSON defaultOptions ''TestnetBalanceOptions
deriveFromJSON defaultOptions ''GenesisInitializer
deriveFromJSON defaultOptions ''ProtocolConstants
deriveFromJSON defaultOptions ''GenesisSpec
