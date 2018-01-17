module Pos.Crypto.Signing.Types.Redeem
       ( RedeemSecretKey (..)
       , RedeemPublicKey (..)
       , RedeemSignature (..)
       , redeemPkB64F
       , redeemPkB64UrlF
       , redeemPkB64ShortF
       , fromAvvmPk
       , redeemPkBuild
       , redeemToPublic
       ) where

import qualified Data.ByteString as BS
import           Data.Hashable (Hashable)
import qualified Data.Text as T
import qualified Data.Text.Buildable as B
import           Formatting (Format, bprint, fitLeft, later, (%), (%.))
import           Serokell.Util.Base64 (formatBase64)
import qualified Serokell.Util.Base64 as B64
import           Universum

import qualified Crypto.Sign.Ed25519 as Ed25519

----------------------------------------------------------------------------
-- Underlying wrappers' instances
----------------------------------------------------------------------------

instance Hashable Ed25519.PublicKey
instance Hashable Ed25519.SecretKey
instance Hashable Ed25519.Signature

instance NFData Ed25519.PublicKey
instance NFData Ed25519.SecretKey
instance NFData Ed25519.Signature

----------------------------------------------------------------------------
-- PK/SK and formatters
----------------------------------------------------------------------------

-- | Wrapper around 'Ed25519.PublicKey'.
newtype RedeemPublicKey = RedeemPublicKey Ed25519.PublicKey
    deriving (Eq, Ord, Show, Generic, NFData, Hashable, Typeable)

-- | Wrapper around 'Ed25519.SecretKey'.
newtype RedeemSecretKey = RedeemSecretKey Ed25519.SecretKey
    deriving (Eq, Ord, Show, Generic, NFData, Hashable)

redeemPkB64F :: Format r (RedeemPublicKey -> r)
redeemPkB64F =
    later $ \(RedeemPublicKey pk) -> formatBase64 $ Ed25519.openPublicKey pk

-- | Base64url Format for 'RedeemPublicKey'.
redeemPkB64UrlF :: Format r (RedeemPublicKey -> r)
redeemPkB64UrlF =
    later $ \(RedeemPublicKey pk) ->
        B.build $ B64.encodeUrl $ Ed25519.openPublicKey pk

redeemPkB64ShortF :: Format r (RedeemPublicKey -> r)
redeemPkB64ShortF = fitLeft 8 %. redeemPkB64F

instance B.Buildable RedeemPublicKey where
    build = bprint ("redeem_pk:"%redeemPkB64F)

instance B.Buildable RedeemSecretKey where
    build = bprint ("redeem_sec_of_pk:"%redeemPkB64F) . redeemToPublic

----------------------------------------------------------------------------
-- Redeem signatures
----------------------------------------------------------------------------

-- | Wrapper around 'Ed25519.Signature'.
newtype RedeemSignature a = RedeemSignature Ed25519.Signature
    deriving (Eq, Ord, Show, Generic, NFData, Hashable, Typeable)

instance B.Buildable (RedeemSignature a) where
    build _ = "<redeem signature>"

-- | Public key derivation function.
redeemToPublic :: RedeemSecretKey -> RedeemPublicKey
redeemToPublic (RedeemSecretKey k) = RedeemPublicKey (Ed25519.secretToPublicKey k)

-- | Read the text into a redeeming public key. The key should be in
-- AVVM format which is base64(url). This function must be inverse of
-- redeemPkB64UrlF formatter.
fromAvvmPk :: (MonadFail m) => Text -> m RedeemPublicKey
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

-- | Creates a public key from 32 byte bytestring, fails with 'error'
-- otherwise.
redeemPkBuild :: ByteString -> RedeemPublicKey
redeemPkBuild bs
    | BS.length bs /= 32 =
        error $
        "consRedeemPk: failed to form pk, wrong bs length: " <> show (BS.length bs) <>
        ", when should be 32"
    | otherwise = RedeemPublicKey $ Ed25519.PublicKey $ bs
