{-# LANGUAGE TemplateHaskell #-}

-- | Binary serialization of Version types (software, block)

module Pos.Binary.Core.Version () where

import           Universum

import           Pos.Binary.Class (Bi (..), Cons (..), Field (..), convertSize,
                                   deriveSimpleBi, getAsciiString1b, label, labelP,
                                   putAsciiString1b, sizeAsciiString1b)
import qualified Pos.Core.Types   as V

instance Bi V.ApplicationName where
    size = convertSize (toString . V.getApplicationName) sizeAsciiString1b
    put (toString . V.getApplicationName -> tag) =
        labelP "ApplicationName" $ putAsciiString1b tag
    get = label "ApplicationName" $ V.mkApplicationName . toText
            =<< getAsciiString1b "SystemTag" V.applicationNameMaxLength

deriveSimpleBi ''V.SoftwareVersion [
    Cons 'V.SoftwareVersion [
        Field 'V.svAppName ''V.ApplicationName,
        Field 'V.svNumber  ''V.NumSoftwareVersion
    ]]

deriveSimpleBi ''V.BlockVersion [
    Cons 'V.BlockVersion [
        Field 'V.bvMajor ''Word16,
        Field 'V.bvMinor ''Word16,
        Field 'V.bvAlt   ''Word8
    ]]
