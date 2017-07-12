-- | Binary serialization of Version types (software, block)

module Pos.Binary.Core.Version () where

import           Universum

import           Pos.Binary.Class (Bi (..), Cons (..), Field (..), convertSize,
                                   deriveSimpleBi, getAsciiString1b, label, labelP,
                                   putAsciiString1b, sizeAsciiString1b)
import qualified Pos.Core.Types   as V
import qualified Pos.Binary.Cbor  as Cbor

instance Bi V.ApplicationName where
    size = convertSize (toString . V.getApplicationName) sizeAsciiString1b
    put (toString . V.getApplicationName -> tag) =
        labelP "ApplicationName" $ putAsciiString1b tag
    get = label "ApplicationName" $ V.mkApplicationName . toText
            =<< getAsciiString1b "SystemTag" V.applicationNameMaxLength

instance Cbor.Bi V.ApplicationName where
  encode appName = Cbor.encode (V.getApplicationName appName)
  decode = do
    appName <- Cbor.decode
    case V.mkApplicationName appName of
      Left e  -> fail e
      Right a -> pure a

deriveSimpleBi ''V.SoftwareVersion [
    Cons 'V.SoftwareVersion [
        Field [| V.svAppName :: V.ApplicationName    |],
        Field [| V.svNumber  :: V.NumSoftwareVersion |]
    ]]

Cbor.deriveSimpleBi ''V.SoftwareVersion [
    Cbor.Cons 'V.SoftwareVersion [
        Cbor.Field [| V.svAppName :: V.ApplicationName    |],
        Cbor.Field [| V.svNumber  :: V.NumSoftwareVersion |]
    ]]

deriveSimpleBi ''V.BlockVersion [
    Cons 'V.BlockVersion [
        Field [| V.bvMajor :: Word16 |],
        Field [| V.bvMinor :: Word16 |],
        Field [| V.bvAlt   :: Word8  |]
    ]]

Cbor.deriveSimpleBi ''V.BlockVersion [
    Cbor.Cons 'V.BlockVersion [
        Cbor.Field [| V.bvMajor :: Word16 |],
        Cbor.Field [| V.bvMinor :: Word16 |],
        Cbor.Field [| V.bvAlt   :: Word8  |]
    ]]
