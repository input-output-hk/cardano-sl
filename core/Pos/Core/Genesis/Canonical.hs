-- | Canonical encoding of 'GenesisData'.

module Pos.Core.Genesis.Canonical
       (
       ) where

import           Universum

import           Data.Fixed                 (Fixed (..))
import qualified Data.HashMap.Strict        as HM
import           Data.Time.Units            (Millisecond)
import           Formatting                 (formatToString)
import           Serokell.Data.Memory.Units (Byte)
import           Serokell.Util.Base16       (base16F)
import           Serokell.Util.Base64       (base64F)
import           Text.JSON.Canonical        (JSValue (..), ToJSON (..), ToObjectKey (..),
                                             mkObject)

import           Pos.Binary.Class           (AsBinary (..))
import           Pos.Core.Address           (addressF)
import           Pos.Core.Fee               (Coeff (..), TxFeePolicy (..),
                                             TxSizeLinear (..))
import           Pos.Core.Genesis.Types     (GenesisAvvmBalances (..), GenesisData (..),
                                             GenesisDelegation (..),
                                             GenesisWStakeholders (..),
                                             ProtocolConstants (..))
import           Pos.Core.Types             (Address, BlockVersionData (..), Coin,
                                             CoinPortion, EpochIndex (..),
                                             SharedSeed (..), SoftforkRule (..),
                                             StakeholderId, Timestamp (..),
                                             getCoinPortion, unsafeGetCoin)
import           Pos.Core.Vss               (VssCertificate (..))
import           Pos.Crypto                 (ProxyCert, ProxySecretKey (..), PublicKey,
                                             RedeemPublicKey, Signature,
                                             fullProxyCertHexF, fullPublicKeyF,
                                             fullSignatureHexF, hashHexF, redeemPkB64UrlF)

----------------------------------------------------------------------------
-- Primitive standard/3rdparty types
----------------------------------------------------------------------------

instance Monad m => ToJSON m Int32 where
    toJSON = pure . JSNum . fromIntegral

instance Monad m => ToJSON m Word16 where
    toJSON = pure . JSNum . fromIntegral

instance Monad m => ToJSON m Word32 where
    toJSON = pure . JSNum . fromIntegral

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
-- doesn't necessary fits into JS number.
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

instance Monad m => ToObjectKey m StakeholderId where
    toObjectKey = pure . formatToString hashHexF

instance Monad m => ToJSON m Coin where
    toJSON = toJSON @_ @Word64 . unsafeGetCoin  -- i. e. String

instance Monad m => ToJSON m CoinPortion where
    toJSON = toJSON @_ @Word64 . getCoinPortion  -- i. e. String

-- JSNum is ok here, current timestamp is almost 6x less than maximal
-- bound, so overflow will happen after we die.
instance Monad m => ToJSON m Timestamp where
    toJSON = pure . JSNum . fromIntegral

instance Monad m => ToObjectKey m Address where
    toObjectKey = pure . formatToString addressF

instance Monad m => ToJSON m Address where
    toJSON = fmap JSString . toObjectKey

instance Monad m => ToJSON m (ProxySecretKey EpochIndex) where
    toJSON ProxySecretKey {..} =
        -- omega is encoded as a number, because in genesis we always
        -- set it to 0.
        mkObject
            [ ("omega", pure (JSNum . fromIntegral $ pskOmega))
            , ("issuerPk", toJSON pskIssuerPk)
            , ("delegatePk", toJSON pskDelegatePk)
            , ("cert", toJSON pskCert)
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
    toJSON = toJSON . toList . unGenesisDelegation

instance Monad m => ToJSON m ProtocolConstants where
    toJSON ProtocolConstants {..} =
        mkObject
            -- 'k' definitely won't exceed the limit
            [ ("k", pure . JSNum . fromIntegral $ pcK)
            , ("protocolMagic", toJSON pcProtocolMagic)
            , ("vssMaxTTL", toJSON pcVssMaxTTL)
            , ("vssMinTTL", toJSON pcVssMinTTL)
            ]

instance Monad m => ToJSON m GenesisAvvmBalances where
    toJSON = toJSON . getGenesisAvvmBalances

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
            -- no need to encode keys from 'VssCertificatesMap'
            , ("vssCerts", toJSON $ toList gdVssCerts)
            , ("nonAvvmBalances", toJSON gdNonAvvmBalances)
            , ("blockVersionData", toJSON gdBlockVersionData)
            , ("protocolConsts", toJSON gdProtocolConsts)
            , ("avvmDistr", toJSON gdAvvmDistr)
            , ("ftsSeed", toJSON gdFtsSeed)
            ]
