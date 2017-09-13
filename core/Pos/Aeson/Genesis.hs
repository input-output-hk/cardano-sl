-- | Aeson instances for GenesisSpec and related datatypes.

module Pos.Aeson.Genesis
       ( fromAvvmPk
       ) where

import           Universum

import           Data.Aeson             (FromJSON (..), withObject, (.:))
import           Data.Aeson.TH          (deriveFromJSON)
import           Serokell.Aeson.Options (defaultOptions)

import           Pos.Binary.Core.Address ()
import           Pos.Aeson.Core         ()
import           Pos.Aeson.Crypto       ()
import           Pos.Core.Genesis.Types (AvvmData, AvvmEntry (..), FakeAvvmOptions,
                                         GenesisAvvmBalances,
                                         GenesisNonAvvmBalances, GenesisDelegation,
                                         GenesisInitializer, GenesisSpec,
                                         GenesisWStakeholders, ProtocolConstants,
                                         TestnetBalanceOptions, TestnetDistribution,
                                         convertAvvmDataToBalances,
                                         convertNonAvvmDataToBalances)
import           Pos.Crypto             (fromAvvmPk)

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

deriveFromJSON defaultOptions ''GenesisDelegation
deriveFromJSON defaultOptions ''GenesisWStakeholders
deriveFromJSON defaultOptions ''TestnetDistribution
deriveFromJSON defaultOptions ''FakeAvvmOptions
deriveFromJSON defaultOptions ''TestnetBalanceOptions
deriveFromJSON defaultOptions ''GenesisInitializer
deriveFromJSON defaultOptions ''ProtocolConstants
deriveFromJSON defaultOptions ''GenesisSpec
