module Pos.Core.Version
       ( parseBlockVersion
       , parseSoftwareVersion
       ) where

import           Universum

import           Serokell.Util.Parse    (parseIntegralSafe)
import           Text.Parsec            (parserFail, try)
import           Text.Parsec.Char       (alphaNum, char, letter, string)
import           Text.Parsec.Combinator (manyTill)
import           Text.Parsec.Text       (Parser)

import           Pos.Core.Types         (BlockVersion (..), SoftwareVersion (..),
                                         mkApplicationName)
import           Pos.Util.Util          ()

parseBlockVersion :: Parser BlockVersion
parseBlockVersion = do
    bvMajor <- parseIntegralSafe
    _       <- char '.'
    bvMinor <- parseIntegralSafe
    _       <- char '.'
    bvAlt   <- parseIntegralSafe
    return BlockVersion{..}

parseSoftwareVersion :: Parser SoftwareVersion
parseSoftwareVersion = do
    svAppName <-
        either parserFail pure . mkApplicationName . toText =<<
        ((:) <$> letter <*> manyTill (alphaNum <|> char '-') (try $ string ":"))
    svNumber <- parseIntegralSafe
    return SoftwareVersion {..}
