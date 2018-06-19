{-# LANGUAGE NoImplicitPrelude, LambdaCase #-}
module Main where

import           Universum

import System.IO (stdin)
import Data.ByteString.Lazy.Char8 as L8
import Text.JSON.Canonical (JSValue, parseCanonicalJSON, renderCanonicalJSON)
import Crypto.Hash (Digest, hashlazy)
import Crypto.Hash.Algorithms (Blake2b_256)

main :: IO ()
main = do
  f <- parseArgs
  val <- readJSON f
  print (blakeHash val)

parseArgs :: IO (Maybe FilePath)
parseArgs = getArgs >>= \case
  ["-"] -> pure Nothing
  [f] -> pure (Just f)
  _ -> die "usage: genesis-hash INFILE.json" >> exitFailure

readJSON :: Maybe FilePath -> IO JSValue
readJSON mf = do
  bs <- case mf of
    Just f -> L8.readFile f
    Nothing -> L8.hGetContents stdin
  case parseCanonicalJSON bs of
    Right v -> pure v
    Left e -> die e >> exitFailure

blakeHash :: JSValue -> Digest Blake2b_256
blakeHash = hashlazy . renderCanonicalJSON
