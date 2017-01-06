{-# LANGUAGE UndecidableInstances #-}

-- | Binary serialization of Pos.Types.Version module

module Pos.Binary.Version () where

import           Data.Binary.Get   (label)
import qualified Data.Text         as T
import           Universum

import           Pos.Binary.Class  (Bi (..))
import           Pos.Binary.Util   (getAsciiString1b, putAsciiString1b)
import qualified Pos.Types.Version as V

instance Bi V.ApplicationName where
    get = label "ApplicationName" $ V.mkApplicationName =<< T.pack
            <$> getAsciiString1b "SystemTag" V.applicationNameMaxLength
    put (T.unpack . V.getApplicationName -> tag) = putAsciiString1b tag

instance Bi V.SoftwareVersion where
    get = label "SoftwareVersion" $ V.SoftwareVersion <$> get <*> get
    put V.SoftwareVersion {..} =  put svAppName
                               *> put svNumber

instance Bi V.ProtocolVersion where
    get = label "ProtocolVersion" $ V.ProtocolVersion <$> get <*> get <*> get
    put V.ProtocolVersion {..} =  put pvMajor
                               *> put pvMinor
                               *> put pvAlt
