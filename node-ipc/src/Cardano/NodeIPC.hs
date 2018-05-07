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
import qualified Data.ByteString.Lazy      as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC
import           Foreign.C.Types           (CInt)
import           GHC.IO.Handle.FD          (fdToHandle)
import           GHC.Generics              (Generic)
import           Pos.Shutdown.Logic        (triggerShutdown)
import           Pos.Shutdown.Class        (HasShutdownContext (..))
import           Pos.Shutdown.Types        (ShutdownContext)
import           System.Environment        (lookupEnv)
import           System.IO.Error           (IOError, isEOFError)
import           System.IO                 (hFlush, hGetLine, hSetNewlineMode, noNewlineTranslation)
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
      BSL.hPut handle $ (encode cmd) <> "\n"
      hFlush handle
    action :: Packet -> m ()
    action QueryPort = do
      send $ ReplyPort port
    action foo = logInfo $ "Unhandled IPC msg: " <> show foo
  let
    loop :: m ()
    loop = do
      send Started
      forever $ do
        line <- liftIO $ hGetLine handle
        let
          packet :: Either String Packet
          packet = eitherDecode $ BSLC.pack line
          handlePacket :: Either String Packet -> m ()
          handlePacket (Left err) = send $ ParseError $ toText err
          handlePacket (Right cmd) = action cmd
        handlePacket packet
    handler :: IOError -> m ()
    handler err = do
      logError $ "exception caught in NodeIPC: " <> (show err)
      when (isEOFError err) $ logError "its an eof"
      liftIO $ hFlush stdout
      triggerShutdown
  catch loop handler
