{-# LANGUAGE TemplateHaskell #-}

module Pos.Types.Version
       (
         -- * Protocol Version
         BlockVersion (..)
       , canBeNextPV
       , parseBlockVersion

         -- * Software Version
       , NumSoftwareVersion
       , SoftwareVersion (..)
       , ApplicationName (..)
       , mkApplicationName
       , applicationNameMaxLength
       , parseSoftwareVersion
       ) where

import           Universum              hiding (show)

import           Data.Char              (isAscii)
import           Data.Hashable          (Hashable)
import           Data.SafeCopy          (base, deriveSafeCopySimple)
import qualified Data.Text              as T
import qualified Data.Text.Buildable    as Buildable
import           Formatting             (bprint, int, shown, stext, (%))
import           Prelude                (show)
import           Text.Parsec            (try)
import           Text.Parsec.Char       (anyChar, char, letter, string)
import           Text.Parsec.Combinator (manyTill)
import           Text.Parsec.Text       (Parser)

import           Pos.Util               (parseIntegralSafe)

-- | Communication protocol version.
data BlockVersion = BlockVersion
    { bvMajor :: !Word16
    , bvMinor :: !Word16
    , bvAlt   :: !Word8
    } deriving (Eq, Generic, Ord, Typeable)

instance Show BlockVersion where
    show BlockVersion {..} =
        intercalate "." [show bvMajor, show bvMinor, show bvAlt]

instance Buildable BlockVersion where
    build = bprint shown

-- | This function checks whether block version passed as the
-- second argument can be adopted after adoption of block version
-- passed as the first argument.
canBeNextPV :: BlockVersion -> BlockVersion -> Bool
canBeNextPV BlockVersion { bvMajor = oldMajor
                         , bvMinor = oldMinor
                         , bvAlt = oldAlt}
            BlockVersion { bvMajor = newMajor
                         , bvMinor = newMinor
                         , bvAlt = newAlt}
    | oldMajor /= newMajor = and [newMajor == oldMajor + 1, newMinor == 0]
    | otherwise = or [ newMinor == oldMinor + 1 && newAlt == oldAlt + 1
                     , newMinor == oldMinor + 1 && newAlt == oldAlt
                     , newMinor == oldMinor && newAlt == oldAlt + 1
                     ]

parseBlockVersion :: Parser BlockVersion
parseBlockVersion = do
    bvMajor <- parseIntegralSafe
    _       <- char '.'
    bvMinor <- parseIntegralSafe
    _       <- char '.'
    bvAlt   <- parseIntegralSafe
    return BlockVersion{..}

instance Hashable BlockVersion

newtype ApplicationName = ApplicationName
    { getApplicationName :: Text
    } deriving (Eq, Ord, Show, Generic, Typeable, ToString, Hashable, Buildable)

applicationNameMaxLength :: Integral i => i
applicationNameMaxLength = 10

mkApplicationName :: MonadFail m => Text -> m ApplicationName
mkApplicationName appName
    | T.length appName > applicationNameMaxLength =
        fail "ApplicationName: too long string passed"
    | T.any (not . isAscii) appName =
        fail "ApplicationName: not ascii string passed"
    | otherwise = pure $ ApplicationName appName

-- | Numeric software version associated with ApplicationName.
type NumSoftwareVersion = Word32

-- | Software version.
data SoftwareVersion = SoftwareVersion
    { svAppName :: !ApplicationName
    , svNumber  :: !NumSoftwareVersion
    } deriving (Eq, Generic, Ord, Typeable)

instance Buildable SoftwareVersion where
    build SoftwareVersion {..} =
      bprint (stext % ":" % int)
         (getApplicationName svAppName) svNumber

instance Show SoftwareVersion where
    show = toString . pretty

instance Hashable SoftwareVersion

parseSoftwareVersion :: Parser SoftwareVersion
parseSoftwareVersion = do
    svAppName <- ApplicationName . toText <$>
        ((:) <$> letter <*> manyTill anyChar (try $ string "-"))
    svNumber  <- parseIntegralSafe
    return SoftwareVersion{..}

deriveSafeCopySimple 0 'base ''ApplicationName
deriveSafeCopySimple 0 'base ''BlockVersion
deriveSafeCopySimple 0 'base ''SoftwareVersion
