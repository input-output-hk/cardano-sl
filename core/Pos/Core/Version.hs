module Pos.Core.Version
       ( mkApplicationName
       , parseBlockVersion
       , applicationNameMaxLength
       , parseSoftwareVersion
       ) where

import           Data.Char              (isAscii)
import qualified Data.Text              as T
import           Serokell.Util.Parse    (parseIntegralSafe)
import           Text.Parsec            (try)
import           Text.Parsec.Char       (anyChar, char, letter, string)
import           Text.Parsec.Combinator (manyTill)
import           Text.Parsec.Text       (Parser)
import           Universum

import           Pos.Core.Types         (ApplicationName (..), BlockVersion (..),
                                         SoftwareVersion (..))

mkApplicationName :: MonadFail m => Text -> m ApplicationName
mkApplicationName appName
    | T.length appName > applicationNameMaxLength =
        fail "ApplicationName: too long string passed"
    | T.any (not . isAscii) appName =
        fail "ApplicationName: not ascii string passed"
    | otherwise = pure $ ApplicationName appName

parseBlockVersion :: Parser BlockVersion
parseBlockVersion = do
    bvMajor <- parseIntegralSafe
    _       <- char '.'
    bvMinor <- parseIntegralSafe
    _       <- char '.'
    bvAlt   <- parseIntegralSafe
    return BlockVersion{..}

applicationNameMaxLength :: Integral i => i
applicationNameMaxLength = 10

parseSoftwareVersion :: Parser SoftwareVersion
parseSoftwareVersion = do
    svAppName <- ApplicationName . toText <$>
        ((:) <$> letter <*> manyTill anyChar (try $ string "-"))
    svNumber  <- parseIntegralSafe
    return SoftwareVersion{..}
