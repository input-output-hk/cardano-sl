{-# LANGUAGE TemplateHaskell #-}

module Pos.Update.Version
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

import qualified Control.Monad          as Monad
import           Data.Char              (isAscii)
import           Data.Hashable          (Hashable)
import           Data.SafeCopy          (base, deriveSafeCopySimple)
import qualified Data.Text              as T
import qualified Data.Text.Buildable    as Buildable
import           Formatting             (bprint, int, shown, stext, (%))
import           Prelude                (read, show)
import           Serokell.Util.Parse    (parseIntegralSafe)
import           Text.Parsec            (try)
import           Text.Parsec.Char       (anyChar, char, letter, string)
import           Text.Parsec.Combinator (manyTill)
import           Text.Parsec.Text       (Parser)
import           Universum              hiding (show)

import           Pos.Core.Types         (ApplicationName (..), BlockVersion (..),
                                         NumSoftwareVersion, SoftwareVersion (..))

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
