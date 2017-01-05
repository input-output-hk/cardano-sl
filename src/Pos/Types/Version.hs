{-# LANGUAGE TemplateHaskell #-}

module Pos.Types.Version
       (
         -- * Protocol Version
         ProtocolVersion (..)
       , canBeNextPV

         -- * Software Version
       , SoftwareVersion (..)
       , ApplicationName (..)
       , mkApplicationName
       , applicationNameMaxLength
       ) where

import           Data.Char           (isAscii)
import           Data.Hashable       (Hashable)
import           Data.SafeCopy       (base, deriveSafeCopySimple)
import qualified Data.Text           as T
import qualified Data.Text.Buildable as Buildable
import           Formatting          (bprint, build, shown, stext, (%))
import           Prelude             (show)
import           Universum           hiding (show)

-- | Communication protocol version.
data ProtocolVersion = ProtocolVersion
    { pvMajor :: Word16
    , pvMinor :: Word16
    , pvAlt   :: Word8
    } deriving (Eq, Generic, Ord, Typeable)

instance Show ProtocolVersion where
    show ProtocolVersion {..} =
        intercalate "." [show pvMajor, show pvMinor, show pvAlt]

instance Buildable ProtocolVersion where
    build = bprint shown

-- | This function checks whether protocol version passed as the
-- second argument can be approved after approval of protocol version
-- passed as the first argument.
canBeNextPV :: ProtocolVersion -> ProtocolVersion -> Bool
canBeNextPV ProtocolVersion { pvMajor = oldMajor
                            , pvMinor = oldMinor
                            , pvAlt = oldAlt}
            ProtocolVersion { pvMajor = newMajor
                            , pvMinor = newMinor
                            , pvAlt = newAlt}
    | oldMajor /= newMajor = and [newMajor == oldMajor + 1, newMinor == 0]
    | otherwise = or [ newMinor == oldMinor + 1 && newAlt == oldAlt + 1
                     , newMinor == oldMinor + 1 && newAlt == oldAlt
                     , newMinor == oldMinor && newAlt == oldAlt + 1
                     ]

instance Hashable ProtocolVersion

newtype ApplicationName = ApplicationName
    { getApplicationName :: Text
    } deriving (Eq, Ord, Show, Generic, Typeable, ToString, Hashable)

applicationNameMaxLength :: Integral i => i
applicationNameMaxLength = 10

mkApplicationName :: MonadFail m => Text -> m ApplicationName
mkApplicationName appName
    | T.length appName > applicationNameMaxLength =
        fail "ApplicationName: too long string passed"
    | T.any (not . isAscii) appName =
        fail "ApplicationName: not ascii string passed"
    | otherwise = pure $ ApplicationName appName

-- | Software version.
data SoftwareVersion = SoftwareVersion
    { svAppName :: ApplicationName
    , svMajor   :: Word8
    , svMinor   :: Word16
    }
  deriving (Eq, Generic, Ord, Typeable)

instance Buildable SoftwareVersion where
    build SoftwareVersion {..} =
      bprint (stext % ":" % build % "." % build)
         (getApplicationName svAppName) svMajor svMinor

instance Show SoftwareVersion where
    show SoftwareVersion {..} = mconcat
        [ toString svAppName
        , ":"
        , show svMajor
        , "."
        , show svMinor
        ]

instance Hashable SoftwareVersion

deriveSafeCopySimple 0 'base ''ApplicationName
deriveSafeCopySimple 0 'base ''ProtocolVersion
deriveSafeCopySimple 0 'base ''SoftwareVersion
