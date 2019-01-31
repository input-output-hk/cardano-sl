{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module BlockParser (printBlock) where

import           Codec.CBOR.Read (deserialiseFromBytes)
import           Pos.Binary.Class (decode)
import           Pos.Chain.Block (Block)
import           System.IO hiding (print)
import           Universum hiding (openFile, when)

import qualified Data.ByteString.Lazy as LBS

printBlock :: FilePath -> IO ()
printBlock filename = do
  raw <- LBS.readFile filename
  let
    blockraw :: LBS.ByteString
    _undoraw :: LBS.ByteString
    Right ("", (blockraw, _undoraw)) = deserialiseFromBytes decode raw
    block :: Block
    Right ("", block) = deserialiseFromBytes decode blockraw
  case block of
    Left gb -> do
      hnd <- openFile "show.txt" WriteMode
      print ("genesis block" :: String)
      hPutStrLn hnd $ show gb
      hClose hnd
    Right mb -> do
      print ("main block" :: String)
      print mb
