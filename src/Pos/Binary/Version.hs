{-# LANGUAGE UndecidableInstances #-}

-- | Binary serialization of Pos.Types.Version module

module Pos.Binary.Version () where

import           Data.Binary.Get   (label)
import           Universum

import           Pos.Binary.Class  (Bi (..))
import           Pos.Binary.Util   (getAsciiString1b, putAsciiString1b)
import qualified Pos.Types.Version as V

instance Bi V.ApplicationName where
    get = label "ApplicationName" $ V.mkApplicationName . toText
            =<< getAsciiString1b "SystemTag" V.applicationNameMaxLength
    put (toString . V.getApplicationName -> tag) = putAsciiString1b tag

instance Bi V.SoftwareVersion where
    get = label "SoftwareVersion" $ V.SoftwareVersion <$> get <*> get
    put V.SoftwareVersion {..} =  put svAppName
                               *> put svNumber

instance Bi V.ProtocolVersion where
    get = label "ProtocolVersion" $ V.ProtocolVersion <$> get <*> get <*> get
    put V.ProtocolVersion {..} =  put pvMajor
                               *> put pvMinor
                               *> put pvAlt
