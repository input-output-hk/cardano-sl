{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pos.Crypto.SafeCopy
       ( getCopyBi
       , putCopyBi
       ) where

import           Universum

import qualified Cardano.Crypto.Wallet as CC
import qualified Cardano.Crypto.Wallet.Encrypted as CC
import           Crypto.Hash (HashAlgorithm)
import           Data.SafeCopy (SafeCopy (..), base, contain,
                     deriveSafeCopySimple, safeGet, safePut)
import           Pos.Binary.Class (AsBinary (..), Bi)
import qualified Pos.Binary.Class as Bi
import           Pos.Binary.SafeCopy (getCopyBi, putCopyBi)
import           Pos.Crypto (AbstractHash, WithHash (..))
import           Pos.Crypto.Signing.Redeem (RedeemPublicKey (..),
                     RedeemSecretKey (..), RedeemSignature (..))
import           Pos.Crypto.Signing.Signing (ProxyCert (..),
                     ProxySecretKey (..), ProxySignature (..), PublicKey (..),
                     SecretKey (..), Signature (..), Signed (..))

import           Pos.Util.Util (cerealError)


deriveSafeCopySimple 0 'base ''CC.ChainCode
deriveSafeCopySimple 0 'base ''CC.EncryptedKey

deriveSafeCopySimple 0 'base ''CC.XSignature
deriveSafeCopySimple 0 'base ''CC.XPub
deriveSafeCopySimple 0 'base ''CC.XPrv

deriveSafeCopySimple 0 'base ''PublicKey
deriveSafeCopySimple 0 'base ''SecretKey

deriveSafeCopySimple 0 'base ''ProxySecretKey

deriveSafeCopySimple 0 'base ''RedeemPublicKey
deriveSafeCopySimple 0 'base ''RedeemSecretKey


instance SafeCopy (AsBinary a) where
    getCopy = contain $ AsBinary <$> safeGet
    putCopy = contain . safePut . getAsBinary

instance (HashAlgorithm algo, Typeable algo, Typeable a) => SafeCopy (AbstractHash algo a) where
   putCopy = putCopyBi
   getCopy = getCopyBi

instance (Typeable a, Bi a) => SafeCopy (WithHash a) where
    getCopy = getCopyBi
    putCopy = putCopyBi

instance SafeCopy (RedeemSignature a) where
    putCopy (RedeemSignature sig) = contain $ safePut sig
    getCopy = contain $ RedeemSignature <$> safeGet


instance SafeCopy (Signature a) where
    putCopy (Signature sig) = contain $ safePut sig
    getCopy = contain $ Signature <$> safeGet

instance Bi a => SafeCopy (Signed a) where
    putCopy (Signed v s) = contain $ safePut (Bi.serialize' (v,s))
    getCopy = contain $ do
        bs <- safeGet
        case Bi.decodeFull bs of
            Left err    -> cerealError $ "getCopy@SafeCopy: " <> err
            Right (v,s) -> pure $ Signed v s

instance SafeCopy (ProxyCert w) where
    putCopy (ProxyCert sig) = contain $ safePut sig
    getCopy = contain $ ProxyCert <$> safeGet

instance SafeCopy w => SafeCopy (ProxySignature w a) where
    putCopy ProxySignature{..} = contain $ do
        safePut psigPsk
        safePut psigSig
    getCopy = contain $ ProxySignature <$> safeGet <*> safeGet
