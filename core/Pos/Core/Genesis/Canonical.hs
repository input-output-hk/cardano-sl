-- | Canonical encoding of 'GenesisData'.

module Pos.Core.Genesis.Canonical
       ( SchemaError(..)
       ) where

import           Universum

import           Control.Lens (_Left)
import           Control.Monad.Except (MonadError (..))
import           Data.Fixed (Fixed (..))
import qualified Data.HashMap.Strict as HM
import qualified Data.Text.Buildable as Buildable
import qualified Data.Text.Lazy.Builder as Builder (fromText)
import           Data.Time.Units (Millisecond, Second, convertUnit)
import           Data.Typeable (typeRep)
import           Formatting (formatToString)
import           Serokell.Data.Memory.Units (Byte)
import           Serokell.Util.Base16 (base16F)
import qualified Serokell.Util.Base16 as B16
import           Serokell.Util.Base64 (base64F)
import qualified Serokell.Util.Base64 as B64
import           Serokell.Util.Text (readDecimal, readUnsignedDecimal)
import           Text.JSON.Canonical (FromJSON (..), FromObjectKey (..), Int54, JSValue (..),
                                      ReportSchemaErrors (expected), ToJSON (..), ToObjectKey (..),
                                      expectedButGotValue, fromJSField, fromJSObject, mkObject)

import           Pos.Binary.Class (AsBinary (..))
import           Pos.Binary.Core.Address ()
import           Pos.Core.Common (Address, Coeff (..), Coin (..), CoinPortion (..), SharedSeed (..),
                                  StakeholderId, TxFeePolicy (..), TxSizeLinear (..), addressF,
                                  decodeTextAddress, getCoinPortion, unsafeGetCoin)
import           Pos.Core.Delegation (HeavyDlgIndex (..), ProxySKHeavy)
import           Pos.Core.Genesis.Helpers (recreateGenesisDelegation)
import           Pos.Core.Genesis.Types (GenesisAvvmBalances (..), GenesisData (..),
                                         GenesisDelegation (..), GenesisNonAvvmBalances (..),
                                         GenesisVssCertificatesMap (..), GenesisWStakeholders (..),
                                         GenesisProtocolConstants (..))
import           Pos.Core.ProtocolConstants (VssMaxTTL (..), VssMinTTL (..))
import           Pos.Core.Slotting.Types (EpochIndex (..), Timestamp (..))
import           Pos.Core.Ssc.Types (VssCertificate (..), VssCertificatesMap (..))
import           Pos.Core.Ssc.Vss (validateVssCertificatesMap)
import           Pos.Core.Update.Types (BlockVersionData (..), SoftforkRule (..))
import           Pos.Crypto (ProxyCert, ProxySecretKey (..), PublicKey, RedeemPublicKey, Signature,
                             decodeAbstractHash, fromAvvmPk, fullProxyCertHexF, fullPublicKeyF,
                             fullSignatureHexF, hashHexF, parseFullProxyCert, parseFullPublicKey,
                             parseFullSignature, redeemPkB64UrlF)
import           Pos.Crypto.Configuration (ProtocolMagic (..))

----------------------------------------------------------------------------
-- Primitive standard/3rdparty types
----------------------------------------------------------------------------

data SchemaError = SchemaError
    { seExpected :: !Text
    , seActual   :: !(Maybe Text)
    } deriving (Show)

instance Buildable SchemaError where
    build SchemaError{..} = mconcat
        [ "expected " <> Builder.fromText seExpected
        , case seActual of
            Nothing     -> mempty
            Just actual -> " but got " <> Builder.fromText actual
        ]

instance (Monad m, Applicative m, MonadError SchemaError m) => ReportSchemaErrors m where
    expected expec actual = throwError SchemaError
        { seExpected = fromString expec
        , seActual = fmap fromString actual
        }

instance Monad m => ToJSON m Int32 where
    toJSON = pure . JSNum . fromIntegral

instance Monad m => ToJSON m Word16 where
    toJSON = pure . JSNum . fromIntegral

instance Monad m => ToJSON m Word32 where
    toJSON = pure . JSNum . fromIntegral

instance Monad m => ToJSON m VssMaxTTL where
    toJSON = toJSON . getVssMaxTTL

instance Monad m => ToJSON m VssMinTTL where
    toJSON = toJSON . getVssMinTTL

instance Monad m => ToJSON m Word64 where
    toJSON = pure . JSString . show

instance Monad m => ToJSON m Integer where
    toJSON = pure . JSString . show

-- We don't need to sort keys in this instance, because rendering
-- takes care of it.
instance (Monad m, ToObjectKey m k, ToJSON m a) => ToJSON m (HashMap k a) where
    toJSON = fmap JSObject . mapM aux . HM.toList
      where
        aux :: (k, a) -> m (String, JSValue)
        aux (k, a) = (,) <$> toObjectKey k <*> toJSON a

instance Monad m => ToJSON m Byte where
    toJSON = toJSON . toInteger

instance Monad m => ToJSON m Millisecond where
    toJSON = toJSON . toInteger

----------------------------------------------------------------------------
-- Our crypto types
----------------------------------------------------------------------------

instance Monad m => ToJSON m PublicKey where
    toJSON = pure . JSString . formatToString fullPublicKeyF

instance Monad m => ToJSON m (ProxyCert w) where
    toJSON = pure . JSString . formatToString fullProxyCertHexF

instance Monad m => ToJSON m (Signature w) where
    toJSON = pure . JSString . formatToString fullSignatureHexF

instance Monad m => ToObjectKey m RedeemPublicKey where
    toObjectKey = pure . formatToString redeemPkB64UrlF

----------------------------------------------------------------------------
-- Our core types
----------------------------------------------------------------------------

instance Monad m => ToJSON m (AsBinary smth) where
    toJSON = pure . JSString . formatToString base64F . getAsBinary

-- Note that it will be encoded as string, because 'EpochIndex'
-- doesn't necessary fit into JS number.
instance Monad m => ToJSON m EpochIndex where
    toJSON = toJSON . getEpochIndex

instance Monad m => ToJSON m VssCertificate where
    toJSON vc =
        mkObject
            [ ("vssKey", toJSON (vcVssKey vc))
            , ("expiryEpoch", pure (JSNum . fromIntegral $ vcExpiryEpoch vc))
            , ("signature", toJSON (vcSignature vc))
            , ("signingKey", toJSON (vcSigningKey vc))
            ]

instance Monad m => ToJSON m VssCertificatesMap where
    toJSON = toJSON . getVssCertificatesMap

instance Monad m => ToObjectKey m StakeholderId where
    toObjectKey = pure . formatToString hashHexF

instance Monad m => ToJSON m Coin where
    toJSON = toJSON @_ @Word64 . unsafeGetCoin  -- i. e. String

instance Monad m => ToJSON m CoinPortion where
    toJSON = toJSON @_ @Word64 . getCoinPortion  -- i. e. String

-- In genesis we don't need microseconds precision, we represent
-- timestamps as seconds for convenience.
instance Monad m => ToJSON m Timestamp where
    toJSON (Timestamp microsec) =
        pure $ JSNum $ fromIntegral @Second (convertUnit microsec)

instance Monad m => ToObjectKey m Address where
    toObjectKey = pure . formatToString addressF

instance Monad m => ToJSON m Address where
    toJSON = fmap JSString . toObjectKey

instance Monad m => ToJSON m ProxySKHeavy where
    toJSON psk =
        -- omega is encoded as a number, because in genesis we always
        -- set it to 0.
        mkObject
            [ ("omega", pure (JSNum . fromIntegral . getHeavyDlgIndex $ pskOmega psk))
            , ("issuerPk", toJSON $ pskIssuerPk psk)
            , ("delegatePk", toJSON $ pskDelegatePk psk)
            , ("cert", toJSON $ pskCert psk)
            ]

instance Monad m => ToJSON m SoftforkRule where
    toJSON SoftforkRule {..} =
        mkObject
            [ ("initThd", toJSON srInitThd)
            , ("minThd", toJSON srMinThd)
            , ("thdDecrement", toJSON srThdDecrement)
            ]

instance Monad m => ToJSON m Coeff where
    toJSON (Coeff (MkFixed integer)) = toJSON @_ @Integer integer

instance Monad m => ToJSON m TxFeePolicy where
    toJSON (TxFeePolicyTxSizeLinear (TxSizeLinear summand multiplier)) =
        mkObject
            [("summand", toJSON summand), ("multiplier", toJSON multiplier)]
    toJSON (TxFeePolicyUnknown {}) =
        error "Having TxFeePolicyUnknown in genesis is likely a bug"

instance Monad m => ToJSON m GenesisWStakeholders where
    toJSON (GenesisWStakeholders stks) = toJSON stks

instance Monad m => ToJSON m GenesisDelegation where
    toJSON = toJSON . unGenesisDelegation

instance Monad m => ToJSON m GenesisProtocolConstants where
    toJSON GenesisProtocolConstants {..} =
        mkObject
            -- 'k' definitely won't exceed the limit
            [ ("k", pure . JSNum . fromIntegral $ gpcK)
            , ("protocolMagic", toJSON (getProtocolMagic gpcProtocolMagic))
            , ("vssMaxTTL", toJSON gpcVssMaxTTL)
            , ("vssMinTTL", toJSON gpcVssMinTTL)
            ]

instance Monad m => ToJSON m GenesisAvvmBalances where
    toJSON = toJSON . getGenesisAvvmBalances

instance Monad m => ToJSON m GenesisVssCertificatesMap where
    toJSON = toJSON . getGenesisVssCertificatesMap

instance Monad m => ToJSON m GenesisNonAvvmBalances where
    toJSON = toJSON . getGenesisNonAvvmBalances

instance Monad m => ToJSON m SharedSeed where
    toJSON (SharedSeed seed) = pure . JSString . formatToString base16F $ seed

instance Monad m => ToJSON m BlockVersionData where
    toJSON (BlockVersionData scriptVersion slotDuration maxBlockSize maxHeaderSize maxTxSize maxProposalSize mpcThd heavyDelThd updateVoteThd updateProposalThd updateImplicit softforkRule txFeePolicy unlockStakeEpoch) =
        mkObject
            [ ("scriptVersion", toJSON scriptVersion)
            , ("slotDuration", toJSON slotDuration)
            , ("maxBlockSize", toJSON maxBlockSize)
            , ("maxHeaderSize", toJSON maxHeaderSize)
            , ("maxTxSize", toJSON maxTxSize)
            , ("maxProposalSize", toJSON maxProposalSize)
            , ("mpcThd", toJSON mpcThd)
            , ("heavyDelThd", toJSON heavyDelThd)
            , ("updateVoteThd", toJSON updateVoteThd)
            , ("updateProposalThd", toJSON updateProposalThd)
            , ("updateImplicit", toJSON updateImplicit)
            , ("softforkRule", toJSON softforkRule)
            , ("txFeePolicy", toJSON txFeePolicy)
            , ("unlockStakeEpoch", toJSON unlockStakeEpoch)
            ]

instance Monad m => ToJSON m GenesisData where
    toJSON GenesisData {..} =
        mkObject
            [ ("bootStakeholders", toJSON gdBootStakeholders)
            , ("heavyDelegation", toJSON gdHeavyDelegation)
            , ("startTime", toJSON gdStartTime)
            , ("vssCerts", toJSON gdVssCerts)
            , ("nonAvvmBalances", toJSON gdNonAvvmBalances)
            , ("blockVersionData", toJSON gdBlockVersionData)
            , ("protocolConsts", toJSON gdProtocolConsts)
            , ("avvmDistr", toJSON gdAvvmDistr)
            , ("ftsSeed", toJSON gdFtsSeed)
            ]




----------------------------------------------------------------------------
-- Parsing
----------------------------------------------------------------------------

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

tryParseString ::
       forall a m e. (Typeable a, ReportSchemaErrors m, ToString e)
    => (Text -> Either e a)
    -> JSValue
    -> m a
tryParseString parser =
    \case
        JSString str ->
            case parser (toText str) of
                Right res -> pure res
                Left (toString -> err) ->
                    expected typeName (Just $ str <> ", err was: " <> err)
        val -> expectedButGotValue typeName val
  where
    typeName = show $ typeRep (Proxy @a)

wrapConstructor ::
       forall e a m. (Typeable a, ReportSchemaErrors m, ToString e)
    => Either e a
    -> m a
wrapConstructor =
    \case
        Left err ->
            expected typeName (Just $ "error occurred: " <> toString err)
        Right x -> pure x
  where
    typeName = show $ typeRep (Proxy @a)

----------------------------------------------------------------------------
-- External
---------------------------------------------------------------------------

instance (ReportSchemaErrors m) => FromJSON m Int32 where
    fromJSON (JSNum i) = pure . fromIntegral $ i
    fromJSON val       = expectedButGotValue "Int32" val

instance (ReportSchemaErrors m) => FromJSON m Word16 where
    fromJSON (JSNum i) = pure . fromIntegral $ i
    fromJSON val       = expectedButGotValue "Word16" val

instance (ReportSchemaErrors m) => FromJSON m Word32 where
    fromJSON (JSNum i) = pure . fromIntegral $ i
    fromJSON val       = expectedButGotValue "Word32" val

instance (ReportSchemaErrors m) => FromJSON m VssMaxTTL where
    fromJSON = fmap VssMaxTTL . fromJSON

instance (ReportSchemaErrors m) => FromJSON m VssMinTTL where
    fromJSON = fmap VssMinTTL . fromJSON

instance (ReportSchemaErrors m) => FromJSON m Word64 where
    fromJSON = tryParseString readUnsignedDecimal

instance (ReportSchemaErrors m) => FromJSON m Integer where
    fromJSON = tryParseString readDecimal

instance (ReportSchemaErrors m, Eq k, Hashable k, FromObjectKey m k, FromJSON m a) =>
         FromJSON m (HashMap k a) where
    fromJSON enc = do
        obj <- fromJSObject enc
        HM.fromList . catMaybes <$> mapM aux obj
      where
        aux :: (String, JSValue) -> m (Maybe (k, a))
        aux (k, a) = knownKeys <$> fromObjectKey k <*> fromJSON a
        knownKeys :: Maybe k -> a -> Maybe (k, a)
        knownKeys Nothing _  = Nothing
        knownKeys (Just k) a = Just (k, a)

instance ReportSchemaErrors m => FromJSON m Byte where
    fromJSON = fmap fromInteger . fromJSON

instance ReportSchemaErrors m => FromJSON m Millisecond where
    fromJSON = fmap fromInteger . fromJSON

----------------------------------------------------------------------------
-- Crypto
----------------------------------------------------------------------------

instance ReportSchemaErrors m => FromJSON m PublicKey where
    fromJSON = tryParseString parseFullPublicKey

instance (Typeable w, ReportSchemaErrors m) => FromJSON m (ProxyCert w) where
    fromJSON = tryParseString parseFullProxyCert

instance (Typeable x, ReportSchemaErrors m) => FromJSON m (Signature x) where
    fromJSON = tryParseString parseFullSignature

instance ReportSchemaErrors m => FromObjectKey m RedeemPublicKey where
    fromObjectKey =
        fmap Just .
        tryParseString (over _Left pretty . fromAvvmPk) .
        JSString

instance ReportSchemaErrors m => FromJSON m (AsBinary smth) where
    fromJSON = fmap AsBinary . tryParseString B64.decode

instance ReportSchemaErrors m => FromJSON m EpochIndex where
    fromJSON = fmap EpochIndex . fromJSON

instance (ReportSchemaErrors m) => FromJSON m VssCertificate where
    fromJSON obj = do
        vssKey <- fromJSField obj "vssKey"
        expiryEpoch <- fromIntegral @Int54 <$> fromJSField obj "expiryEpoch"
        signature <- fromJSField obj "signature"
        signingKey <- fromJSField obj "signingKey"
        return $ UnsafeVssCertificate
            { vcVssKey      = vssKey
            , vcExpiryEpoch = expiryEpoch
            , vcSignature   = signature
            , vcSigningKey  = signingKey
            }

instance ReportSchemaErrors m => FromJSON m VssCertificatesMap where
    fromJSON val = do
        m <- UnsafeVssCertificatesMap <$> fromJSON val
        wrapConstructor (validateVssCertificatesMap m)

instance ReportSchemaErrors m => FromObjectKey m StakeholderId where
    fromObjectKey = fmap Just . tryParseString (decodeAbstractHash) . JSString

instance ReportSchemaErrors m => FromJSON m Coin where
    fromJSON = fmap Coin . fromJSON

instance ReportSchemaErrors m => FromJSON m CoinPortion where
    fromJSON val = do
        number <- fromJSON val
        pure $ CoinPortion number

instance ReportSchemaErrors m => FromJSON m Timestamp where
    fromJSON =
        fmap (Timestamp . convertUnit @Second . fromIntegral) .
        fromJSON @_ @Int54

instance ReportSchemaErrors m => FromObjectKey m Address where
    fromObjectKey = fmap Just . tryParseString decodeTextAddress . JSString

instance ReportSchemaErrors m => FromJSON m Address where
    fromJSON = tryParseString decodeTextAddress

instance ReportSchemaErrors m => FromJSON m ProxySKHeavy where
    fromJSON obj = do
        pskOmega <- HeavyDlgIndex . fromIntegral @Int54 <$> fromJSField obj "omega"
        pskIssuerPk <- fromJSField obj "issuerPk"
        pskDelegatePk <- fromJSField obj "delegatePk"
        pskCert <- fromJSField obj "cert"
        pure UnsafeProxySecretKey{..}

instance ReportSchemaErrors m => FromJSON m SoftforkRule where
    fromJSON obj = do
        srInitThd <- fromJSField obj "initThd"
        srMinThd <- fromJSField obj "minThd"
        srThdDecrement <- fromJSField obj "thdDecrement"
        return SoftforkRule {..}

instance ReportSchemaErrors m => FromJSON m Coeff where
    fromJSON = fmap (Coeff . MkFixed) . fromJSON @_ @Integer

instance ReportSchemaErrors m => FromJSON m TxFeePolicy where
    fromJSON obj = do
        summand <- fromJSField obj "summand"
        multiplier <- fromJSField obj "multiplier"
        return $ TxFeePolicyTxSizeLinear (TxSizeLinear summand multiplier)

instance ReportSchemaErrors m => FromJSON m GenesisWStakeholders where
    fromJSON = fmap GenesisWStakeholders . fromJSON

instance ReportSchemaErrors m => FromJSON m GenesisDelegation where
    fromJSON val = do
        psks <- fromJSON val
        wrapConstructor $ recreateGenesisDelegation psks

instance ReportSchemaErrors m => FromJSON m GenesisProtocolConstants where
    fromJSON obj = do
        gpcK <- fromIntegral @Int54 <$> fromJSField obj "k"
        gpcProtocolMagic <- ProtocolMagic <$> fromJSField obj "protocolMagic"
        gpcVssMaxTTL <- fromJSField obj "vssMaxTTL"
        gpcVssMinTTL <- fromJSField obj "vssMinTTL"
        return GenesisProtocolConstants {..}

instance ReportSchemaErrors m => FromJSON m GenesisAvvmBalances where
    fromJSON = fmap GenesisAvvmBalances . fromJSON

instance ReportSchemaErrors m => FromJSON m GenesisNonAvvmBalances where
    fromJSON = fmap GenesisNonAvvmBalances . fromJSON

instance ReportSchemaErrors m => FromJSON m SharedSeed where
    fromJSON = fmap SharedSeed . tryParseString B16.decode

instance ReportSchemaErrors m => FromJSON m BlockVersionData where
    fromJSON obj = do
        bvdScriptVersion <- fromJSField obj "scriptVersion"
        bvdSlotDuration <- fromJSField obj "slotDuration"
        bvdMaxBlockSize <- fromJSField obj "maxBlockSize"
        bvdMaxHeaderSize <- fromJSField obj "maxHeaderSize"
        bvdMaxTxSize <- fromJSField obj "maxTxSize"
        bvdMaxProposalSize <- fromJSField obj "maxProposalSize"
        bvdMpcThd <- fromJSField obj "mpcThd"
        bvdHeavyDelThd <- fromJSField obj "heavyDelThd"
        bvdUpdateVoteThd <- fromJSField obj "updateVoteThd"
        bvdUpdateProposalThd <- fromJSField obj "updateProposalThd"
        bvdUpdateImplicit <- fromJSField obj "updateImplicit"
        bvdSoftforkRule <- fromJSField obj "softforkRule"
        bvdTxFeePolicy <- fromJSField obj "txFeePolicy"
        bvdUnlockStakeEpoch <- fromJSField obj "unlockStakeEpoch"
        return BlockVersionData {..}

instance (ReportSchemaErrors m) => FromJSON m GenesisData where
    fromJSON obj = do
        gdBootStakeholders <- fromJSField obj "bootStakeholders"
        gdHeavyDelegation <- fromJSField obj "heavyDelegation"
        gdStartTime <- fromJSField obj "startTime"
        -- note that we don't need to validate this map explicitly because
        -- the FromJSON instance of 'VssCertificatesMap' already does this
        gdVssCerts <- GenesisVssCertificatesMap <$> fromJSField obj "vssCerts"
        gdNonAvvmBalances <- fromJSField obj "nonAvvmBalances"
        gdBlockVersionData <- fromJSField obj "blockVersionData"
        gdProtocolConsts <- fromJSField obj "protocolConsts"
        gdAvvmDistr <- fromJSField obj "avvmDistr"
        gdFtsSeed <- fromJSField obj "ftsSeed"
        return GenesisData {..}
