{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE DeriveGeneric #-}

module Cardano.NodeIPC (startNodeJsIPC) where

import System.Environment
import Control.Concurrent
import Data.Maybe
import Control.Monad
import System.IO
import GHC.IO.Handle.FD
import Foreign.C.Types
import Control.Exception
import System.IO.Error
import Data.Aeson
import Data.Aeson.Types
import qualified Data.HashMap.Lazy as HML
import GHC.Generics
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC
import Data.Monoid
import Data.Word

startNodeJsIPC :: Word16 -> IO ()
startNodeJsIPC port = do
  let
    action QueryPort reply = do
      reply $ ReplyPort port
    action foo _ = print foo
  maybeFd <- lookupEnv "NODE_CHANNEL_FD"
  when (isJust maybeFd) $ do
    let fd = read $ fromJust maybeFd
    thread <- forkIO $ ipcListener fd action
    pure ()

data Packet = Started | QueryPort | ReplyPort Word16 | Ping | Pong | ParseError String deriving (Show, Eq, Generic)

opts :: Options
opts = defaultOptions { sumEncoding = ObjectWithSingleField }

instance FromJSON Packet where
  parseJSON = genericParseJSON opts

instance ToJSON Packet where
  toEncoding = genericToEncoding opts

ipcListener :: CInt -> (Packet -> (Packet -> IO () ) -> IO () ) -> IO ()
ipcListener fd action = do
  --handle <- mkHandleFromFD fd Stream "IPC" ReadWriteMode False Nothing
  handle <- fdToHandle fd
  let
    send :: Packet -> IO ()
    send cmd = do
      BSL.hPut handle $ (encode cmd) <> "\n"
      hFlush handle
    loop :: IO ()
    loop = do
      line <- hGetLine handle
      let
        packet :: Either String Packet
        packet = eitherDecode $ BSLC.pack line
        handlePacket (Left err) = send $ ParseError err
        handlePacket (Right cmd) = action cmd send
      handlePacket packet
      loop
    handler :: IOError -> IO ()
    handler err = do
      print "end of file"
      when (isEOFError err) $ print "its an eof"
    start :: IO ()
    start = do
      send Started
      loop
  catch start handler
