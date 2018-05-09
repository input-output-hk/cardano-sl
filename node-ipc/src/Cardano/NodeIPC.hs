{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall            #-}

module Cardano.NodeIPC (startNodeJsIPC) where

import           Control.Arrow             ( (>>>) )
import           Control.Concurrent
import           Control.Exception ()
import           Control.Monad.Reader (MonadReader)
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Aeson.Types          (Options, SumEncoding(ObjectWithSingleField), sumEncoding)
import           Data.Binary.Get           (runGet, getWord64le, getWord32le)
import           Data.Binary.Put           (runPut, putWord32le, putWord64le, putLazyByteString)
import qualified Data.ByteString.Lazy      as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC
import           Distribution.System       (buildOS, OS(Windows))
import           Foreign.C.Types           (CInt)
import           GHC.IO.Handle.FD          (fdToHandle)
import           GHC.Generics              (Generic)
import           Pos.Shutdown.Logic        (triggerShutdown)
import           Pos.Shutdown.Class        (HasShutdownContext (..))
import           Pos.Shutdown.Types        (ShutdownContext)
import           System.Environment        (lookupEnv)
import           System.IO.Error           (IOError, isEOFError)
import           System.IO                 (hFlush, hGetLine, hSetNewlineMode, noNewlineTranslation, hGetChar)
import           System.Wlog               (WithLogger, logInfo, logError)
import           System.Wlog.LoggerNameBox (usingLoggerName)
import           Universum

data Packet = Started | QueryPort | ReplyPort Word16 | Ping | Pong | ParseError Text deriving (Show, Eq, Generic)

opts :: Options
opts = defaultOptions { sumEncoding = ObjectWithSingleField }

instance FromJSON Packet where
  parseJSON = genericParseJSON opts

instance ToJSON Packet where
  toEncoding = genericToEncoding opts

startNodeJsIPC ::
    (MonadIO m, WithLogger m, MonadReader ctx m, HasShutdownContext ctx)
    => Word16 -> m ()
startNodeJsIPC port = void $ runMaybeT $ do
  ctx <- view shutdownContext
  fdstring <- liftIO (lookupEnv "NODE_CHANNEL_FD") >>= (pure >>> MaybeT)
  case readEither fdstring of
    Left err -> lift $ logError $ "unable to parse NODE_CHANNEL_FD: " <> err
    Right fd -> void $ liftIO $ forkIO $ startIpcListener ctx fd port

startIpcListener :: ShutdownContext -> CInt -> Word16 -> IO ()
startIpcListener ctx fd port = usingLoggerName "NodeIPC" $ flip runReaderT ctx (ipcListener fd port)

ipcListener ::
    forall m ctx . (MonadCatch m, MonadIO m, WithLogger m, MonadReader ctx m, HasShutdownContext ctx)
    => CInt -> Word16 -> m ()
ipcListener fd port = do
  handle <- liftIO $ fdToHandle fd
  liftIO $ hSetNewlineMode handle noNewlineTranslation
  let
    send :: Packet -> m ()
    send cmd = liftIO $ do
      sendMessage $ encode cmd
    action :: Packet -> m ()
    action QueryPort = do
      send $ ReplyPort port
    action foo = logInfo $ "Unhandled IPC msg: " <> show foo
    sendMessage :: BSL.ByteString -> IO ()
    sendMessage blob = do
      if buildOS == Windows
        then sendWindowsMessage 1 0 (blob <> "\n")
        else sendLinuxMessage blob
      hFlush handle
    sendWindowsMessage :: Word32 -> Word32 -> BSL.ByteString -> IO ()
    sendWindowsMessage int1 int2 blob = do
      let
        message = runPut $ (putWord32le int1) <> (putWord32le int2) <> (putWord64le $ fromIntegral $ BSL.length blob) <> (putLazyByteString blob)
      BSLC.hPut handle message
    sendLinuxMessage :: BSL.ByteString -> IO ()
    sendLinuxMessage blob = do
      BSLC.hPutStrLn handle blob
  let
    loop :: m ()
    loop = do
      send Started
      forever $ do
        line <- readMessage
        let
          handlePacket :: Either String Packet -> m ()
          handlePacket (Left err) = send $ ParseError $ toText err
          handlePacket (Right cmd) = action cmd
        handlePacket $ eitherDecode line
    handler :: IOError -> m ()
    handler err = do
      logError $ "exception caught in NodeIPC: " <> (show err)
      when (isEOFError err) $ logError "its an eof"
      liftIO $ hFlush stdout
      triggerShutdown
    debugloop :: m ()
    debugloop = forever $ do
        char <- liftIO $ hGetChar handle
        logInfo $ "got char '" <> (show char) <> "' (" <> (show $ ord char) <> ")"
    readMessage :: m BSL.ByteString
    readMessage = do
      if buildOS == Windows
        then do
          (int1, int2, blob) <- liftIO windowsReadMessage
          logInfo $ "int is: " <> (show [int1, int2]) <> " and blob is: " <> (show blob)
          return blob
        else
          liftIO linuxReadMessage
    linuxReadMessage :: IO BSL.ByteString
    linuxReadMessage = do
      line <- hGetLine handle
      return $ BSLC.pack line
    windowsReadMessage :: IO (Word32, Word32, BSL.ByteString)
    windowsReadMessage = do
      int1 <- readInt32 handle
      int2 <- readInt32 handle
      size <- readInt64 handle
      blob <- BSL.hGet handle $ fromIntegral size
      return (int1, int2, blob)
    readInt64 :: Handle -> IO Word64
    readInt64 hnd = do
      bs <- BSL.hGet hnd 8
      pure $ runGet getWord64le bs
    readInt32 hnd = do
      bs <- BSL.hGet hnd 4
      pure $ runGet getWord32le bs
  catch loop handler
