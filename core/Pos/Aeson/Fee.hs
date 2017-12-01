-- | Aeson instances for 'TxFeePolicy' and its subtypes.

module Pos.Aeson.Fee
       (
       ) where

import           Universum

import qualified Data.Aeson as JSON
import qualified Data.Aeson.Types as JSON
import           Data.Fixed (Fixed (..), resolution)
import qualified Data.HashMap.Strict as HM.S

import           Pos.Core.Common.Fee (Coeff (..), TxFeePolicy (..), TxSizeLinear (..))

instance JSON.FromJSON Coeff where
    parseJSON = JSON.withScientific "Coeff" $ \sc -> do
        -- Code below is resistant to changes in precision of 'Coeff'.
        let
            rat = toRational sc * toRational res
            fxd = MkFixed (numerator rat)
            res = resolution fxd
            bad = denominator rat /= 1
        when bad $
            fail "Fixed precision for coefficient exceeded"
        return $ Coeff fxd

instance JSON.FromJSON TxSizeLinear where
    parseJSON = JSON.withObject "TxSizeLinear" $ \o -> do
        TxSizeLinear
            <$> (o JSON..: "a")
            <*> (o JSON..: "b")

instance JSON.FromJSON TxFeePolicy where
    parseJSON = JSON.withObject "TxFeePolicy" $ \o -> do
        (policyName, policyBody) <- case HM.S.toList o of
            []  -> fail "TxFeePolicy: none provided"
            [a] -> pure a
            _   -> fail "TxFeePolicy: ambiguous choice"
        let
          policyParser :: JSON.FromJSON p => JSON.Parser p
          policyParser = JSON.parseJSON policyBody
        case policyName of
            "txSizeLinear" ->
                TxFeePolicyTxSizeLinear <$> policyParser
            "unknown" ->
                mkTxFeePolicyUnknown <$> policyParser
            _ ->
                fail "TxFeePolicy: unknown policy name"
        where
            mkTxFeePolicyUnknown (policyTag, policyPayload) =
                TxFeePolicyUnknown policyTag
                    (encodeUtf8 @Text @ByteString policyPayload)
