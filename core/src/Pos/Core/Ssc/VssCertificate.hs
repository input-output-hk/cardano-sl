module Pos.Core.Ssc.VssCertificate
       ( VssCertificate (..)

       , _vcVssKey
       , _vcExpiryEpoch
       , _vcSignature
       , _vcSigningKey

       , mkVssCertificate
       , checkVssCertificate
       , checkCertSign
       , getCertId
       , toCertPair
       ) where

import           Universum

import           Control.Lens (makeLensesFor)
import           Control.Monad.Except (MonadError (throwError))
import qualified Data.Aeson.Options as S (defaultOptions)
import           Data.Aeson.TH (deriveJSON)
import           Data.Hashable (Hashable (..))
import           Data.SafeCopy (base, deriveSafeCopySimple)
import           Formatting (bprint, build, int, (%))
import qualified Formatting.Buildable as Buildable
import           Pos.Core.Common (StakeholderId, addressHash)
import           Text.JSON.Canonical (FromJSON (..), Int54, JSValue (..),
                     ReportSchemaErrors, ToJSON (..), fromJSField, mkObject)

import           Pos.Binary.Class (AsBinary, Bi (..), encodeListLen,
                     enforceSize)
import           Pos.Core.Genesis.Canonical ()
import           Pos.Core.Slotting (EpochIndex)
import           Pos.Crypto (ProtocolMagic, PublicKey, SecretKey,
                     SignTag (SignVssCert), Signature, VssPublicKey, checkSig,
                     sign, toPublic)

-- | VssCertificate allows VssPublicKey to participate in MPC. Each
-- stakeholder should create a Vss keypair, sign VSS public key with signing
-- key and send it into blockchain.
--
-- A public key of node is included in certificate in order to enable
-- validation of it using only node's P2PKH address. Expiry epoch is last
-- epoch when certificate is valid, expiry epoch is included in certificate
-- and signature.
--
-- Other nodes accept this certificate if it is valid and if node has enough
-- stake.
--
-- Invariant: 'checkSig vcSigningKey (vcVssKey, vcExpiryEpoch) vcSignature'.
data VssCertificate = UnsafeVssCertificate
    { vcVssKey      :: !(AsBinary VssPublicKey)
    , vcExpiryEpoch :: !EpochIndex
    -- ^ Epoch up to which certificate is valid.
    , vcSignature   :: !(Signature (AsBinary VssPublicKey, EpochIndex))
    , vcSigningKey  :: !PublicKey
    } deriving (Show, Eq, Generic)

flip makeLensesFor ''VssCertificate
  [ ("vcVssKey"     , "_vcVssKey")
  , ("vcExpiryEpoch", "_vcExpiryEpoch")
  , ("vcSignature"  , "_vcSignature")
  , ("vcSigningKey" , "_vcSigningKey")
  ]

instance NFData VssCertificate

instance Ord VssCertificate where
    compare a b = toTuple a `compare` toTuple b
      where
        toTuple UnsafeVssCertificate {..} =
            (vcExpiryEpoch, vcVssKey, vcSigningKey, vcSignature)

instance Buildable VssCertificate where
    build UnsafeVssCertificate {..} = bprint
        ("vssCert:"%build%":"%int) vcSigningKey vcExpiryEpoch

instance Hashable VssCertificate where
    hashWithSalt s UnsafeVssCertificate{..} =
        hashWithSalt s (vcExpiryEpoch, vcVssKey, vcSigningKey, vcSignature)

instance Bi VssCertificate where
    encode vssCert = encodeListLen 4 <> encode (vcVssKey vssCert)
                                     <> encode (vcExpiryEpoch vssCert)
                                     <> encode (vcSignature vssCert)
                                     <> encode (vcSigningKey vssCert)
    decode = do
        enforceSize "VssCertificate" 4
        key <- decode
        epo <- decode
        sig <- decode
        sky <- decode
        pure $ UnsafeVssCertificate key epo sig sky

instance Monad m => ToJSON m VssCertificate where
    toJSON vc =
        mkObject
            [ ("vssKey", toJSON (vcVssKey vc))
            , ("expiryEpoch", pure (JSNum . fromIntegral $ vcExpiryEpoch vc))
            , ("signature", toJSON (vcSignature vc))
            , ("signingKey", toJSON (vcSigningKey vc))
            ]

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

deriveJSON S.defaultOptions ''VssCertificate

-- | Make VssCertificate valid up to given epoch using 'SecretKey' to sign
-- data.
mkVssCertificate
    :: ProtocolMagic
    -> SecretKey
    -> AsBinary VssPublicKey
    -> EpochIndex
    -> VssCertificate
mkVssCertificate pm sk vk expiry =
    UnsafeVssCertificate vk expiry signature (toPublic sk)
  where
    signature = sign pm SignVssCert sk (vk, expiry)

-- | Check a 'VssCertificate' for validity.
checkVssCertificate
    :: (MonadError Text m)
    => ProtocolMagic
    -> VssCertificate
    -> m ()
checkVssCertificate pm it =
    unless (checkCertSign pm it) $ throwError "checkVssCertificate: invalid sign"

-- CHECK: @checkCertSign
-- | Check that the VSS certificate is signed properly
-- #checkPubKeyAddress
-- #checkSig
checkCertSign :: ProtocolMagic -> VssCertificate -> Bool
checkCertSign pm UnsafeVssCertificate {..} =
    checkSig pm SignVssCert vcSigningKey (vcVssKey, vcExpiryEpoch) vcSignature

getCertId :: VssCertificate -> StakeholderId
getCertId = addressHash . vcSigningKey

toCertPair :: VssCertificate -> (StakeholderId, VssCertificate)
toCertPair vc = (getCertId vc, vc)

deriveSafeCopySimple 0 'base ''VssCertificate
