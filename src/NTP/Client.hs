{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RankNTypes            #-}

-- | This module implements functionality of NTP client.

module NTP.Client
    ( startNtpClient
    , NtpClientSettings (..)
    , NtpStopButton (..)
    ) where

import           Control.Applicative         (optional)
import           Control.Concurrent          (threadDelay)
import           Control.Concurrent.STM      (atomically)
import           Control.Concurrent.STM.TVar (TVar, newTVarIO, readTVar, readTVarIO,
                                              writeTVar)
import           Control.Lens                (to, (^?), _head)
import           Control.Monad               (forM_, forever, unless, void)
import           Control.Monad.Catch         (Exception, SomeException (..))
import           Control.Monad.Trans         (MonadIO (..))
import           Data.Binary                 (decodeOrFail, encode)
import qualified Data.ByteString.Lazy        as LBS
import           Data.Default                (Default (..))
import           Data.Monoid                 ((<>))
import           Data.Text                   (Text)
import           Data.Time.Units             (Microsecond)
import           Data.Typeable               (Typeable)
import           Data.Word                   (Word16)
import           Formatting                  (sformat, shown, (%))
import           Network.Socket              (AddrInfo (..), AddrInfoFlag (AI_ADDRCONFIG),
                                              Family (AF_INET), PortNumber, SockAddr (..),
                                              Socket, SocketOption (ReuseAddr),
                                              SocketType (Datagram), addrFamily, close,
                                              defaultHints, defaultProtocol, getAddrInfo,
                                              setSocketOption, socket)
import           Network.Socket.ByteString   (recvFrom, sendManyTo)
import           Prelude                     hiding (log)
import           System.Wlog                 (LoggerName, Severity (..), WithLogger,
                                              logMessage, modifyLoggerName,
                                              usingLoggerName)

import Mockable.Class      (Mockable)
import Mockable.Concurrent (Fork, fork)
import Mockable.Exception  (Catch, Throw, catch, catchAll, throw)
import Mockable.Production (runProduction)
import NTP.Packet          (NtpPacket (..), mkCliNtpPacket)
import NTP.Util            (datagramPacketSize, resolveNtpHost)

type StopWorker m = m ()

data NtpClientSettings = NtpClientSettings
    { ntpServers :: [String]
    , ntpHandler :: forall m . ( MonadIO m, WithLogger m )
                 => Microsecond -> m ()
    , ntpLogName :: LoggerName
    }

data NtpClient = NtpClient
    { ncSocket   :: TVar Socket
    , ncClosed   :: TVar Bool
    , ncSettings :: NtpClientSettings
    }

mkNtpClient :: MonadIO m => NtpClientSettings -> Socket -> m NtpClient
mkNtpClient ncSettings sock = liftIO $ do
    ncSocket <- newTVarIO sock
    ncClosed <- newTVarIO False
    return NtpClient{..}

instance Default NtpClientSettings where
    def = NtpClientSettings
        { ntpServers = ["ntp5.stratum2.ru"]
        , ntpHandler = \_ -> return ()
        , ntpLogName = "ntp-cli"
        }

newtype NtpStopButton = NtpStopButton
    { press :: forall m . ( MonadIO m, WithLogger m
                          , Mockable Fork m, Mockable Throw m, Mockable Catch m )
                          => m ()
    }

newtype FailedToResolveHost = FailedToResolveHost String
    deriving (Show, Typeable)

instance Exception FailedToResolveHost

type NtpMonad m =
    ( MonadIO m
    , WithLogger m
    , Mockable Fork m
    , Mockable Throw m
    , Mockable Catch m
    )

log :: NtpMonad m => NtpClient -> Severity -> Text -> m ()
log cli severity msg = do
    closed <- liftIO $ readTVarIO (ncClosed cli)
    unless closed $ do
        let logName = ntpLogName (ncSettings cli)
        modifyLoggerName (<> logName) $ logMessage severity msg

doSend :: NtpMonad m => SockAddr -> NtpClient -> m ()
doSend addr cli = do
    sock <- liftIO $ readTVarIO (ncSocket cli)
    let packet = encode mkCliNtpPacket
    liftIO (sendManyTo sock (LBS.toChunks packet) addr) `catchAll` handleE
  where
    -- just log; socket closure is handled by receiver
    handleE e = do
        log cli Warning $ sformat ("Failed to send to "%shown) addr

startSend :: NtpMonad m => [SockAddr] -> NtpClient -> m ()
startSend addrs cli = forever $ do
    closed <- liftIO $ readTVarIO (ncClosed cli)
    unless closed $ do
        forM_ addrs $ \addr -> fork $ doSend addr cli
        liftIO $ threadDelay 1000000

mkSocket :: NtpMonad m => MonadIO m => m Socket
mkSocket = liftIO $ do
    sock <- socket AF_INET Datagram defaultProtocol
    setSocketOption sock ReuseAddr 1
    return sock

doReceive :: NtpMonad m => NtpClient -> m ()
doReceive cli = do
    sock <- liftIO . readTVarIO $ ncSocket cli
    let handler = ntpHandler (ncSettings cli)
    forever $ do
        (received, _) <- liftIO $ recvFrom sock datagramPacketSize
        let eNtpPacket = decodeOrFail $ LBS.fromStrict received
        case eNtpPacket of
            Left  (_, _, err)    ->
                log cli Warning $ sformat ("Error while receiving time: "%shown) err
            Right (_, _, packet) -> do
                log cli Debug $ sformat ("Got packet "%shown) packet
                let time = ntpTime packet
                log cli Debug $ sformat ("Received time "%shown) time
                handler time

startReceive :: NtpMonad m => NtpClient -> m ()
startReceive cli = do
    doReceive cli `catchAll` handleE
  where
    -- got error while receiving data, recreate socket
    handleE e = do
        closed <- liftIO . readTVarIO $ ncClosed cli
        unless closed $ do
            log cli Debug $ sformat ("Socket closed, recreating ("%shown%")") e
            sock <- mkSocket
            closed' <- liftIO . atomically $ do
                writeTVar (ncSocket cli) sock
                readTVar  (ncClosed cli)
            -- extra check in case socket was closed by stopping client
            -- while we recreated socket
            unless closed $
                startReceive cli

stopNtpClient :: NtpMonad m => NtpClient -> m ()
stopNtpClient cli = do
    log cli Info "Stopped"
    sock <- liftIO . atomically $ do
        writeTVar (ncClosed cli) True
        readTVar  (ncSocket cli)

    -- unblock receiving from socket in case no one replies
    liftIO (close sock) `catchAll` \_ -> return ()

startNtpClient :: NtpMonad m => NtpClientSettings -> m NtpStopButton
startNtpClient settings = do
    sock <- mkSocket
    cli <- mkNtpClient settings sock

    fork $ startReceive cli

    addrs <- mapM resolveHost $ ntpServers settings
    void . fork $ startSend addrs cli

    log cli Info "Launched"

    return $ NtpStopButton { press = stopNtpClient cli }
  where
    resolveHost host = do
        maddr <- liftIO $ resolveNtpHost host
        case maddr of
            Nothing   -> throw $ FailedToResolveHost host
            Just addr -> return addr
