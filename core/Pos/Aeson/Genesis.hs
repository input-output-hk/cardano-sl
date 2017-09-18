-- | Aeson instances for GenesisSpec and related datatypes.

module Pos.Aeson.Genesis
       ( fromAvvmPk
       ) where

import           Universum

import           Data.Aeson              (FromJSON (..), withArray, withObject, (.:))
import           Data.Aeson.TH           (deriveFromJSON)
import           Serokell.Aeson.Options  (defaultOptions)

import           Pos.Aeson.Core          ()
import           Pos.Aeson.Crypto        ()
import           Pos.Binary.Core.Address ()
import           Pos.Core.Genesis.Types  (AvvmData, AvvmEntry (..), FakeAvvmOptions,
                                          GenesisAvvmBalances, GenesisDelegation,
                                          GenesisInitializer, GenesisNonAvvmBalances,
                                          GenesisSpec, GenesisWStakeholders,
                                          ProtocolConstants, TestnetBalanceOptions,
                                          TestnetDistribution, convertAvvmDataToBalances,
                                          convertNonAvvmDataToBalances,
                                          mkGenesisDelegation)
import           Pos.Core.Types          (ProxySKHeavy)
import           Pos.Crypto              (fromAvvmPk)
import           Pos.Util.Util           (eitherToFail)

instance FromJSON AvvmEntry where
    parseJSON = withObject "avvmEntry" $ \o -> do
        aeCoin <- (* (1000000 :: Integer)) <$> o .: "coin"
        (addrText :: Text) <- o .: "address"
        aePublicKey <- fromAvvmPk addrText
        return AvvmEntry{..}

instance FromJSON AvvmData

instance FromJSON GenesisAvvmBalances where
    parseJSON v = convertAvvmDataToBalances <$> parseJSON v

instance FromJSON GenesisNonAvvmBalances where
    parseJSON = convertNonAvvmDataToBalances <=< parseJSON

instance FromJSON GenesisDelegation where
    parseJSON = withArray "GenesisDelegation" $ \v -> do
        (elems :: [ProxySKHeavy]) <- mapM parseJSON $ toList v
        eitherToFail $ (mkGenesisDelegation elems :: Either Text GenesisDelegation)

deriveFromJSON defaultOptions ''GenesisWStakeholders
deriveFromJSON defaultOptions ''TestnetDistribution
deriveFromJSON defaultOptions ''FakeAvvmOptions
deriveFromJSON defaultOptions ''TestnetBalanceOptions
deriveFromJSON defaultOptions ''GenesisInitializer
deriveFromJSON defaultOptions ''ProtocolConstants
deriveFromJSON defaultOptions ''GenesisSpec
