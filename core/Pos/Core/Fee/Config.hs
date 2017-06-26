module Pos.Core.Fee.Config
       ( ConfigOf(..)
       ) where

import           Universum

import qualified Data.Aeson          as JSON
import qualified Data.Aeson.Types    as JSON
import           Data.Fixed          (Fixed (..), resolution)
import qualified Data.HashMap.Strict as HM.S

import           Pos.Core.Fee        (Coeff (..), TxFeePolicy (..), TxSizeLinear (..))

-- | A newtype for 'JSON.FromJSON' instances used in a config file. We use
-- it because we don't want those instances to be defined for core types.
newtype ConfigOf a = ConfigOf { getConfigOf :: a }
    deriving (Show, Generic)

instance JSON.FromJSON (ConfigOf Coeff) where
    parseJSON = JSON.withScientific "Coeff" $ \sc -> ConfigOf <$> do
        -- Code below is resistant to changes in precision of 'Coeff'.
        let
            rat = toRational sc * toRational res
            fxd = MkFixed (numerator rat)
            res = resolution fxd
            bad = denominator rat /= 1
        when bad $
            fail "Fixed precision for coefficient exceeded"
        return $ Coeff fxd

instance JSON.FromJSON (ConfigOf TxSizeLinear) where
    parseJSON = JSON.withObject "TxSizeLinear" $ \o -> ConfigOf <$> do
        TxSizeLinear
            <$> (getConfigOf <$> o JSON..: "a")
            <*> (getConfigOf <$> o JSON..: "b")

instance JSON.FromJSON (ConfigOf TxFeePolicy) where
    parseJSON = JSON.withObject "TxFeePolicy" $ \o -> ConfigOf <$> do
        (policyName, policyBody) <- case HM.S.toList o of
            []  -> fail "TxFeePolicy: none provided"
            [a] -> pure a
            _   -> fail "TxFeePolicy: ambiguous choice"
        let
          policyParser :: JSON.FromJSON p => JSON.Parser p
          policyParser = JSON.parseJSON policyBody
        case policyName of
            "txSizeLinear" ->
                TxFeePolicyTxSizeLinear . getConfigOf <$> policyParser
            "unknown" ->
                mkTxFeePolicyUnknown <$> policyParser
            _ ->
                fail "TxFeePolicy: unknown policy name"
        where
            mkTxFeePolicyUnknown (policyTag, policyPayload) =
                TxFeePolicyUnknown policyTag
                    (encodeUtf8 @Text @ByteString policyPayload)
