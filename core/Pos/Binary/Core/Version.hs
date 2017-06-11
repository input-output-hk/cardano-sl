-- | Binary serialization of Version types (software, block)

module Pos.Binary.Core.Version () where

import           Universum

import           Pos.Binary.Class (Bi (..), combineSize, convertSize, getAsciiString1b,
                                   label, putAsciiString1b, putField, sizeAsciiString1b)
import qualified Pos.Core.Types   as V

instance Bi V.ApplicationName where
    size = convertSize (toString . V.getApplicationName) sizeAsciiString1b
    put (toString . V.getApplicationName -> tag) = putAsciiString1b tag
    get = label "ApplicationName" $ V.mkApplicationName . toText
            =<< getAsciiString1b "SystemTag" V.applicationNameMaxLength

instance Bi V.SoftwareVersion where
    sizeNPut = putField V.svAppName <> putField V.svNumber
    get = label "SoftwareVersion" $ V.SoftwareVersion <$> get <*> get

instance Bi V.BlockVersion where
    sizeNPut = putField V.bvMajor
            <> putField V.bvMinor
            <> putField V.bvAlt
    get = label "BlockVersion" $ V.BlockVersion <$> get <*> get <*> get
