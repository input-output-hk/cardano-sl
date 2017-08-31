{-# OPTIONS_GHC -Weverything -Wno-unsafe #-}
{-# LANGUAGE DeriveGeneric #-}

{-# LANGUAGE OverloadedStrings #-}

module Main where

import           System.Environment     (getArgs)
import           Prelude
import qualified Text.Megaparsec            as M
import qualified Text.Megaparsec.Lexer      as M.Lexer
import           Text.Megaparsec.String     (Parser)
import Data.Void
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Text.Encoding as T
import           Data.String as S
import           Data.Aeson
import qualified Data.Aeson.Types
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBSC
import qualified Data.ByteString      as BS
import           GHC.Generics
import qualified Data.HashMap.Strict as HM
import Parsehelper

data BlockHeader = BlockHeader {
      slot :: Int,
      epoch :: Int,
      difficulty :: Int
    }

data JournalRow = JournalRow {
      message :: X
    } deriving (Generic, Show)

instance FromJSON JournalRow where
  parseJSON = genericParseJSON opts . jsonLower
    where
      opts = defaultOptions

jsonLower :: Value -> Value
jsonLower (Object o) = Object . HM.fromList . map lowerPair . HM.toList $ o
  where lowerPair (key, val) = (T.toLower key, val)
jsonLower x = x

blockHeaderV = BlockHeader 17 0 18

blockHeaderP :: Parser BlockHeader
blockHeaderP = do
  undefined

temporaryP :: Parser String
temporaryP = do
  undefined

main :: IO ()
main = do
  arg1 : rest <- getArgs
  processLogs arg1
  return ()

processLogs :: FilePath -> IO ()
processLogs fn = do
  rawLog <- Prelude.readFile fn
  let
    rows = S.lines rawLog
    parse = \json -> do
      let
        msg = decode (LBSC.pack json) :: Maybe JournalRow
      case msg of
        Just msg' -> print msg'
        Nothing -> putStrLn json
  mapM parse rows
  return ()
