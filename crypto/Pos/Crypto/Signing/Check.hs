-- | Functions for verifying signatures.
--
-- TODO: the "Pos.Crypto.Signing" hierarchy looks like a mess and should be
-- redesigned. When this is done, we likely won't need this module to be
-- separated from other modules, but right now we do need it in order to
-- avoid circular dependencies. — @neongreen
--
module Pos.Crypto.Signing.Check
       ( checkSig
       , checkSigRaw
       , verifyProxyCert
       , validateProxySecretKey
       ) where

import           Universum

import qualified Cardano.Crypto.Wallet as CC
import           Control.Monad.Except (MonadError, throwError)
import           Data.Coerce (coerce)

import           Pos.Binary.Class (Bi, Raw)
import qualified Pos.Binary.Class as Bi
import           Pos.Crypto.Configuration (HasCryptoConfiguration)
import           Pos.Crypto.Signing.Tag (signTag)
import           Pos.Crypto.Signing.Types (ProxyCert (..), ProxySecretKey (..), PublicKey (..),
                                           SignTag (..), Signature (..))

-- CHECK: @checkSig
-- | Verify a signature.
-- #verifyRaw
checkSig ::
       (HasCryptoConfiguration, Bi a)
    => SignTag
    -> PublicKey
    -> a
    -> Signature a
    -> Bool
checkSig t k x s = checkSigRaw (Just t) k (Bi.serialize' x) (coerce s)

-- CHECK: @checkSigRaw
-- | Verify raw 'ByteString'.
checkSigRaw ::
       HasCryptoConfiguration
    => Maybe SignTag
    -> PublicKey
    -> ByteString
    -> Signature Raw
    -> Bool
checkSigRaw mbTag (PublicKey k) x (Signature s) = CC.verify k (tag <> x) s
  where
    tag = maybe mempty signTag mbTag

-- | Checks if certificate is valid, given issuer pk, delegate pk and ω.
verifyProxyCert :: (HasCryptoConfiguration, Bi w) => PublicKey -> PublicKey -> w -> ProxyCert w -> Bool
verifyProxyCert issuerPk (PublicKey delegatePk) o (ProxyCert sig) =
    checkSig SignProxySK issuerPk
        (mconcat ["00", CC.unXPub delegatePk, Bi.serialize' o])
        (Signature sig)

-- | Return the key if it's valid, and throw an error otherwise.
validateProxySecretKey
    :: (HasCryptoConfiguration, MonadError Text m, Bi w)
    => ProxySecretKey w
    -> m (ProxySecretKey w)
validateProxySecretKey psk =
    if verifyProxyCert (pskIssuerPk psk) (pskDelegatePk psk)
                       (pskOmega psk) (pskCert psk)
        then pure psk
        else throwError "a ProxySecretKey has an invalid signature"
