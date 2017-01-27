{-# LANGUAGE TemplateHaskell #-}

module Pos.Types.Version
       (
         -- * Protocol Version
         BlockVersion (..)
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

import           Pos.Types.Core         (ApplicationName (..), BlockVersion (..),
                                         NumSoftwareVersion, SoftwareVersion (..))
import           Pos.Util               (parseIntegralSafe)

instance Show BlockVersion where
    show BlockVersion {..} =
        intercalate "." [show bvMajor, show bvMinor, show bvAlt]

instance Buildable BlockVersion where
    build = bprint shown

parseBlockVersion :: Parser BlockVersion
parseBlockVersion = do
    bvMajor <- parseIntegralSafe
    _       <- char '.'
    bvMinor <- parseIntegralSafe
    _       <- char '.'
    bvAlt   <- parseIntegralSafe
    return BlockVersion{..}

instance Hashable BlockVersion

applicationNameMaxLength :: Integral i => i
applicationNameMaxLength = 10

mkApplicationName :: MonadFail m => Text -> m ApplicationName
mkApplicationName appName
    | T.length appName > applicationNameMaxLength =
        fail "ApplicationName: too long string passed"
    | T.any (not . isAscii) appName =
        fail "ApplicationName: not ascii string passed"
    | otherwise = pure $ ApplicationName appName

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
