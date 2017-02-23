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

import           Universum              hiding (show)

import qualified Control.Monad          as Monad
import           Data.Char              (isAscii)
import           Data.Hashable          (Hashable)
import           Data.SafeCopy          (base, deriveSafeCopySimple)
import qualified Data.Text              as T
import qualified Data.Text.Buildable    as Buildable
import           Formatting             (bprint, int, shown, stext, (%))
import           Prelude                (read, show)
import           Text.Parsec            (try)
import           Text.Parsec.Char       (anyChar, char, digit, letter, string)
import           Text.Parsec.Combinator (manyTill)
import           Text.Parsec.Text       (Parser)

import           Pos.Types.Core         (ApplicationName (..), BlockVersion (..),
                                         NumSoftwareVersion, SoftwareVersion (..))

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

-- Copied from Pos.Util
--
-- TODO: move to serokell-util
parseIntegralSafe :: Integral a => Parser a
parseIntegralSafe = fromIntegerSafe . read =<< some digit
  where
    fromIntegerSafe :: Integral a => Integer -> Parser a
    fromIntegerSafe x =
        let res = fromInteger x
        in  if fromIntegral res == x
            then return res
            else Monad.fail ("Number is too large: " ++ show x)

deriveSafeCopySimple 0 'base ''ApplicationName
deriveSafeCopySimple 0 'base ''BlockVersion
deriveSafeCopySimple 0 'base ''SoftwareVersion
