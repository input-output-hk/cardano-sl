module Pos.Core.Fee
       ( Coeff(..)
       , TxSizeLinear(..)
       , calculateTxSizeLinear
       , TxFeePolicy(..)
       ) where

import           Universum

import qualified Data.Aeson                 as JSON
import qualified Data.Aeson.Types           as JSON
import           Data.Fixed                 (Fixed (..), Nano, resolution, showFixed)
import qualified Data.Text.Buildable        as Buildable
import           Formatting                 (bprint, build, shown, (%))
import           Serokell.Data.Memory.Units (Byte, toBytes)
import qualified Data.HashMap.Strict        as HM.S

-- | A fractional coefficient of fixed precision.
newtype Coeff = Coeff Nano
    deriving (Eq, Show, Generic, NFData)

instance Buildable Coeff where
    build (Coeff x) = fromString (showFixed True x)

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

-- | A linear equation on the transaction size. Represents the @\s -> a + b*s@
-- function where @s@ is the transaction size, @a@ and @b@ are constant
-- coefficients.
data TxSizeLinear = TxSizeLinear !Coeff !Coeff
    deriving (Eq, Show, Generic)

instance NFData TxSizeLinear

instance Buildable TxSizeLinear where
    build (TxSizeLinear a b) =
        bprint (build%" + "%build%"*s") a b

instance JSON.FromJSON TxSizeLinear where
    parseJSON = JSON.withObject "TxSizeLinear" $ \o ->
        TxSizeLinear
            <$> o JSON..: "a"
            <*> o JSON..: "b"

calculateTxSizeLinear :: TxSizeLinear -> Byte -> Nano
calculateTxSizeLinear
    (TxSizeLinear (Coeff a) (Coeff b))
    (fromInteger . toBytes -> txSize) =
        a + b * txSize

data TxFeePolicy
    = TxFeePolicyTxSizeLinear !TxSizeLinear
    | TxFeePolicyUnknown !Word8 !ByteString
    deriving (Eq, Show, Generic)

instance NFData TxFeePolicy

instance Buildable TxFeePolicy where
    build (TxFeePolicyTxSizeLinear tsp) =
        bprint ("policy(tx-size-linear): "%build) tsp
    build (TxFeePolicyUnknown v bs) =
        bprint ("policy(unknown:"%build%"): "%shown) v bs

instance JSON.FromJSON TxFeePolicy where
    parseJSON = JSON.withObject "TxFeePolicy" $ \o -> do
        (policyName, policyBody) <- case HM.S.toList o of
            [] -> fail "TxFeePolicy: none provided"
            [a] -> pure a
            _ -> fail "TxFeePolicy: ambiguous choice"
        let
          policyParser :: JSON.FromJSON p => JSON.Parser p
          policyParser = JSON.parseJSON policyBody
        case policyName of
            "txSizeLinear" -> TxFeePolicyTxSizeLinear <$> policyParser
            "unknown" -> mkTxFeePolicyUnknown <$> policyParser
            _ -> fail "TxFeePolicy: unknown policy name"
        where
            mkTxFeePolicyUnknown (policyTag, policyPayload) =
                TxFeePolicyUnknown policyTag
                    (encodeUtf8 @Text @ByteString policyPayload)
