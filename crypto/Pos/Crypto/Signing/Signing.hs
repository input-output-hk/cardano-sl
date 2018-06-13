-- | Signing done with public/private keys.

module Pos.Crypto.Signing.Signing
       (
       -- * Keys
         emptyPass
       , keyGen
       , deterministicKeyGen

       -- * Signing and verification
       , sign
       , signEncoded
       , checkSig                      -- reexport
       , mkSigned

       -- * Versions for raw bytestrings
       , signRaw
       , checkSigRaw                   -- reexport

       -- * Proxy signature scheme
       , verifyProxyCert               -- reexport
       , validateProxySecretKey        -- reexport
       , proxySign
       , proxyVerify

       , module Pos.Crypto.Signing.Types.Signing
       ) where

import           Universum

import qualified Cardano.Crypto.Wallet as CC
import           Crypto.Random (MonadRandom, getRandomBytes)
import           Data.ByteArray (ScrubbedBytes)
import qualified Data.ByteString as BS
import           Data.Coerce (coerce)
import           Formatting (build, sformat, (%))

import           Pos.Binary.Class (Bi, Raw)
import qualified Pos.Binary.Class as Bi
import           Pos.Crypto.Configuration (ProtocolMagic)
import           Pos.Crypto.Signing.Check (checkSig, checkSigRaw, validateProxySecretKey,
                                           verifyProxyCert)
import           Pos.Crypto.Signing.Tag (signTag)
import           Pos.Crypto.Signing.Types.Signing
import           Pos.Crypto.Signing.Types.Tag (SignTag)

----------------------------------------------------------------------------
-- Keys, key generation & printing & decoding
----------------------------------------------------------------------------

emptyPass :: ScrubbedBytes
emptyPass = mempty

-- TODO: this is just a placeholder for actual (not ready yet) derivation
-- of keypair from seed in cardano-crypto API
createKeypairFromSeed :: BS.ByteString -> (CC.XPub, CC.XPrv)
createKeypairFromSeed seed =
    let prv = CC.generate seed emptyPass
    in  (CC.toXPub prv, prv)

-- | Generate a key pair. It's recommended to run it with 'runSecureRandom'
-- from "Pos.Crypto.Random" because the OpenSSL generator is probably safer
-- than the default IO generator.
keyGen :: MonadRandom m => m (PublicKey, SecretKey)
keyGen = do
    seed <- getRandomBytes 32
    let (pk, sk) = createKeypairFromSeed seed
    return (PublicKey pk, SecretKey sk)

-- | Create key pair deterministically from 32 bytes.
deterministicKeyGen :: BS.ByteString -> (PublicKey, SecretKey)
deterministicKeyGen seed =
    bimap PublicKey SecretKey (createKeypairFromSeed seed)

----------------------------------------------------------------------------
-- Signatures
----------------------------------------------------------------------------

-- | Encode something with 'Binary' and sign it.
sign
    :: (Bi a)
    => ProtocolMagic
    -> SignTag         -- ^ See docs for 'SignTag'
    -> SecretKey
    -> a
    -> Signature a
sign pm t k = coerce . signRaw pm (Just t) k . Bi.serialize'

-- | Like 'sign' but without the 'Bi' constraint.
signEncoded
    :: ProtocolMagic
    -> SignTag
    -> SecretKey
    -> ByteString
    -> Signature a
signEncoded pm t k = coerce . signRaw pm (Just t) k

-- | Sign a bytestring.
signRaw
    :: ProtocolMagic
    -> Maybe SignTag   -- ^ See docs for 'SignTag'. Unlike in 'sign', we
                       -- allow no tag to be provided just in case you need
                       -- to sign /exactly/ the bytestring you provided
    -> SecretKey
    -> ByteString
    -> Signature Raw
signRaw pm mbTag (SecretKey k) x = Signature (CC.sign emptyPass k (tag <> x))
  where
    tag = maybe mempty (signTag pm) mbTag

-- | Smart constructor for 'Signed' data type with proper signing.
mkSigned :: (Bi a) => ProtocolMagic -> SignTag -> SecretKey -> a -> Signed a
mkSigned pm t sk x = Signed x (sign pm t sk x)

----------------------------------------------------------------------------
-- Proxy signing
----------------------------------------------------------------------------

-- | Make a proxy delegate signature with help of certificate. If the
-- delegate secret key passed doesn't pair with delegate public key in
-- certificate inside, we panic. Please check this condition outside
-- of this function.
proxySign
    :: (Bi a)
    => ProtocolMagic -> SignTag -> SecretKey -> ProxySecretKey w -> a -> ProxySignature w a
proxySign pm t sk@(SecretKey delegateSk) psk m
    | toPublic sk /= pskDelegatePk psk =
        error $ sformat ("proxySign called with irrelevant certificate "%
                         "(psk delegatePk: "%build%", real delegate pk: "%build%")")
                        (pskDelegatePk psk) (toPublic sk)
    | otherwise =
        ProxySignature
        { psigPsk = psk
        , psigSig = sigma
        }
  where
    PublicKey issuerPk = pskIssuerPk psk
    sigma =
        CC.sign emptyPass delegateSk $
        mconcat
            -- it's safe to put the tag after issuerPk because `CC.unXPub
            -- issuerPk` always takes 64 bytes
            ["01", CC.unXPub issuerPk, signTag pm t, Bi.serialize' m]

-- CHECK: @proxyVerify
-- | Verify delegated signature given issuer's pk, signature, message
-- space predicate and message itself.
proxyVerify
    :: (Bi w, Bi a)
    => ProtocolMagic -> SignTag -> ProxySignature w a -> (w -> Bool) -> a -> Bool
proxyVerify pm t ProxySignature{..} omegaPred m =
    predCorrect && sigValid
  where
    PublicKey issuerPk = pskIssuerPk psigPsk
    PublicKey pdDelegatePkRaw = pskDelegatePk psigPsk
    predCorrect = omegaPred (pskOmega psigPsk)
    sigValid =
        CC.verify
            pdDelegatePkRaw
            (mconcat
                 [ "01"
                 , CC.unXPub issuerPk
                 , signTag pm t
                 , Bi.serialize' m
                 ])
            psigSig
