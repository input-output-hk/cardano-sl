-- | Binary serialization of Version types (software, block)

module Pos.Binary.Core.Version () where

import           Universum

import           Pos.Binary.Class (Bi (..), combineSize, convertSize, getAsciiString1b,
                                   label, putAsciiString1b, sizeAsciiString1b)
import qualified Pos.Core.Types   as V

instance Bi V.ApplicationName where
    size = convertSize (toString . V.getApplicationName) sizeAsciiString1b
    put (toString . V.getApplicationName -> tag) = putAsciiString1b tag
    get = label "ApplicationName" $ V.mkApplicationName . toText
            =<< getAsciiString1b "SystemTag" V.applicationNameMaxLength

instance Bi V.SoftwareVersion where
    size = combineSize (V.svAppName, V.svNumber)
    put V.SoftwareVersion {..} = put svAppName *> put svNumber
    get = label "SoftwareVersion" $ V.SoftwareVersion <$> get <*> get

instance Bi V.BlockVersion where
    size = combineSize (V.bvMajor, V.bvMinor, V.bvAlt)
    put V.BlockVersion {..} = put bvMajor *> put bvMinor *> put bvAlt
    get = label "BlockVersion" $ V.BlockVersion <$> get <*> get <*> get
