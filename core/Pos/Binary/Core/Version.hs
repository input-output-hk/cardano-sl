-- | Binary serialization of Version types (software, block)

module Pos.Binary.Core.Version () where

import           Universum

import           Pos.Binary.Class (Bi (..), Cons (..), Field (..), deriveSimpleBi)
import qualified Pos.Core.Types   as V

instance Bi V.ApplicationName where
  encode appName = encode (V.getApplicationName appName)
  decode = do
    appName <- decode
    case V.mkApplicationName appName of
      Left e  -> fail e
      Right a -> pure a

deriveSimpleBi ''V.SoftwareVersion [
    Cons 'V.SoftwareVersion [
        Field [| V.svAppName :: V.ApplicationName    |],
        Field [| V.svNumber  :: V.NumSoftwareVersion |]
    ]]

deriveSimpleBi ''V.BlockVersion [
    Cons 'V.BlockVersion [
        Field [| V.bvMajor :: Word16 |],
        Field [| V.bvMinor :: Word16 |],
        Field [| V.bvAlt   :: Word8  |]
    ]]
