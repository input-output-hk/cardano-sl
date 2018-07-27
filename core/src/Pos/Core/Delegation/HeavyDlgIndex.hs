module Pos.Core.Delegation.HeavyDlgIndex
       ( HeavyDlgIndex (..)
       , ProxySigHeavy
       , ProxySKHeavy
       , ProxySKBlockInfo
       ) where

import           Universum

import qualified Data.Aeson as Aeson (FromJSON (..), ToJSON (..))
import           Data.SafeCopy (SafeCopy (..), contain, safeGet, safePut)
import           Formatting (bprint, build)
import qualified Formatting.Buildable
import           Text.JSON.Canonical (FromJSON (..), Int54, JSValue (..),
                     ReportSchemaErrors, ToJSON (..), fromJSField, mkObject)

import           Pos.Binary.Class (Bi (..))
import           Pos.Core.Slotting (EpochIndex)
import           Pos.Crypto (ProxySecretKey (..), ProxySignature, PublicKey)

-- | Witness for heavy delegation signature -- epoch in which
-- certificate starts being active. It is needed for replay attack
-- prevention (index should match epoch of the block PSK is announced
-- in).
newtype HeavyDlgIndex =
    HeavyDlgIndex { getHeavyDlgIndex :: EpochIndex }
    deriving (Show, Eq, Ord, Generic)

instance NFData HeavyDlgIndex
instance Hashable HeavyDlgIndex

instance Buildable HeavyDlgIndex where
    build (HeavyDlgIndex i) = bprint build i

instance Bi HeavyDlgIndex where
    encode = encode . getHeavyDlgIndex
    decode = HeavyDlgIndex <$> decode

instance SafeCopy HeavyDlgIndex where
    getCopy = contain $ HeavyDlgIndex <$> safeGet
    putCopy x = contain $ safePut $ getHeavyDlgIndex x

instance Aeson.FromJSON HeavyDlgIndex where
    parseJSON v = HeavyDlgIndex <$> Aeson.parseJSON v

instance Aeson.ToJSON HeavyDlgIndex where
    toJSON = Aeson.toJSON . getHeavyDlgIndex

-- | Simple proxy signature without ttl/epoch index constraints.
type ProxySigHeavy a = ProxySignature HeavyDlgIndex a

-- | Heavy delegation PSK.
type ProxySKHeavy = ProxySecretKey HeavyDlgIndex

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

instance ReportSchemaErrors m => FromJSON m ProxySKHeavy where
    fromJSON obj = do
        pskOmega <- HeavyDlgIndex . fromIntegral @Int54 <$> fromJSField obj "omega"
        pskIssuerPk <- fromJSField obj "issuerPk"
        pskDelegatePk <- fromJSField obj "delegatePk"
        pskCert <- fromJSField obj "cert"
        pure UnsafeProxySecretKey{..}

-- | Heavyweight PSK with real leader public key (because heavyweight
-- psks have redelegation feature, so pskIssuerPk hPsk /= leader in
-- general case). This is used to create a block header only.
type ProxySKBlockInfo = Maybe (ProxySKHeavy, PublicKey)
