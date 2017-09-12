-- | Aeson instances for GenesisSpec and related datatypes.

module Pos.Aeson.Genesis
       ( fromAvvmPk
       ) where

import           Universum

import qualified Data.Text               as T
import qualified Data.ByteString         as BS
import           Data.Aeson.TH          (defaultOptions, deriveFromJSON)
import           Data.Aeson             (FromJSON (..), withObject, (.:))
import qualified Serokell.Util.Base64    as B64

import           Pos.Aeson.Crypto ()
import           Pos.Aeson.Core   ()
import           Pos.Core.Types         (EpochIndex)
import           Pos.Crypto             (RedeemPublicKey, redeemPkBuild, ProxySecretKey)
import           Pos.Core.Genesis.Types (GenesisSpec, GenesisAvvmBalances,
                                         AvvmEntry (..), AvvmData, GenesisDelegation,
                                         convertAvvmDataToBalances, ProtocolConstants,
                                         GenesisInitializer, TestnetBalanceOptions, FakeAvvmOptions,
                                         TestnetDistribution, GenesisWStakeholders)
-- | Read the text into a redeeming public key.
--
-- There's also a copy of this function in cardano-addr-convert.
fromAvvmPk :: (MonadFail m, Monad m) => Text -> m RedeemPublicKey
fromAvvmPk addrText = do
    let base64rify = T.replace "-" "+" . T.replace "_" "/"
    let parsedM = B64.decode $ base64rify addrText
    addrParsed <-
        maybe (fail $ "Address " <> toString addrText <> " is not base64(url) format")
        pure
        (rightToMaybe parsedM)
    unless (BS.length addrParsed == 32) $
        fail "Address' length is not equal to 32, can't be redeeming pk"
    pure $ redeemPkBuild addrParsed

instance FromJSON AvvmEntry where
    parseJSON = withObject "avvmEntry" $ \o -> do
        aeCoin <- (* (1000000 :: Integer)) <$> o .: "coin"
        (addrText :: Text) <- o .: "address"
        aePublicKey <- fromAvvmPk addrText
        return AvvmEntry{..}

instance FromJSON AvvmData

instance FromJSON (ProxySecretKey EpochIndex)

instance FromJSON GenesisAvvmBalances where
    parseJSON v = convertAvvmDataToBalances <$> parseJSON v

deriveFromJSON defaultOptions ''GenesisDelegation
deriveFromJSON defaultOptions ''GenesisWStakeholders
deriveFromJSON defaultOptions ''TestnetDistribution
deriveFromJSON defaultOptions ''FakeAvvmOptions
deriveFromJSON defaultOptions ''TestnetBalanceOptions
deriveFromJSON defaultOptions ''GenesisInitializer
deriveFromJSON defaultOptions ''ProtocolConstants
deriveFromJSON defaultOptions ''GenesisSpec
