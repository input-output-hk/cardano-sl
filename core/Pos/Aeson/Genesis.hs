-- | Aeson instances for GenesisSpec and related datatypes.

module Pos.Aeson.Genesis
       (
        -- * FromJSONKey RedeemPublicKey
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
import           Data.Aeson (FromJSON (..), FromJSONKey (..), FromJSONKeyFunction (..))
import           Data.Aeson.TH (deriveFromJSON)
import           Serokell.Aeson.Options (defaultOptions)

import           Pos.Aeson.Core ()
import           Pos.Aeson.Crypto ()
import           Pos.Binary.Core.Address ()
import           Pos.Core.Common (StakeholderId)
import           Pos.Core.Delegation.Types (ProxySKHeavy)
import           Pos.Core.Genesis.Helpers (convertNonAvvmDataToBalances, recreateGenesisDelegation)
import           Pos.Core.Genesis.Types (FakeAvvmOptions, GenesisAvvmBalances (..),
                                         GenesisDelegation, GenesisInitializer,
                                         GenesisNonAvvmBalances, GenesisSpec,
                                         GenesisVssCertificatesMap (..), GenesisWStakeholders (..),
                                         ProtocolConstants, TestnetBalanceOptions)
import           Pos.Core.Ssc (VssCertificatesMap (..), validateVssCertificatesMap)
import           Pos.Crypto (RedeemPublicKey, fromAvvmPk)
import           Pos.Util.Util (toAesonError)

instance FromJSONKey RedeemPublicKey where
    fromJSONKey = FromJSONKeyTextParser (toAesonError . over _Left pretty . fromAvvmPk)
    fromJSONKeyList = FromJSONKeyTextParser (toAesonError . bimap pretty pure . fromAvvmPk)

deriving instance FromJSON GenesisAvvmBalances
deriving instance FromJSON GenesisWStakeholders

instance FromJSON GenesisNonAvvmBalances where
    parseJSON = toAesonError . convertNonAvvmDataToBalances <=< parseJSON

instance FromJSON VssCertificatesMap where
    parseJSON = parseJSON >=>
        toAesonError . validateVssCertificatesMap . UnsafeVssCertificatesMap

instance FromJSON GenesisVssCertificatesMap where
    parseJSON val = GenesisVssCertificatesMap <$> parseJSON val

instance FromJSON GenesisDelegation where
    parseJSON = parseJSON >=> \v -> do
        (elems :: HashMap StakeholderId ProxySKHeavy) <- mapM parseJSON v
        toAesonError $ recreateGenesisDelegation elems

deriveFromJSON defaultOptions ''FakeAvvmOptions
deriveFromJSON defaultOptions ''TestnetBalanceOptions
deriveFromJSON defaultOptions ''GenesisInitializer
deriveFromJSON defaultOptions ''ProtocolConstants
deriveFromJSON defaultOptions ''GenesisSpec
