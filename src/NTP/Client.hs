{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes     #-}

module NTP.Client
    ( startNtpClient
    , NtpClientSettings (..)
    , NtpStopButton (..)
    , def
    ) where

import           Control.Applicative         (optional)
import           Control.Concurrent          (forkIO, threadDelay)
import           Control.Concurrent.STM      (atomically)
import           Control.Concurrent.STM.TVar (TVar, newTVarIO, readTVar, readTVarIO,
                                              writeTVar)
import           Control.Lens                (to, (^?), _head)
import           Control.Monad               (forM_, forever, unless, void)
import           Control.Monad.Catch         (Exception, SomeException (..), catchAll,
                                              throwM)
import           Control.Monad.Trans         (MonadIO (..))
import           Data.Binary                 (decodeOrFail, encode)
import qualified Data.ByteString.Lazy        as LBS
import           Data.Default                (Default (..))
import           Data.Time.Units             (Microsecond)
import           Data.Typeable               (Typeable)
import           Data.Word                   (Word16)
import           Network.Socket              (AddrInfo (..), AddrInfoFlag (AI_ADDRCONFIG),
                                              Family (AF_INET), PortNumber, SockAddr (..),
                                              Socket, SocketOption (ReuseAddr),
                                              SocketType (Datagram), addrFamily, close,
                                              defaultHints, defaultProtocol, getAddrInfo,
                                              setSocketOption, socket)
import           Network.Socket.ByteString   (recvFrom, sendManyTo)
import           Prelude                     hiding (log)
import           System.Wlog                 (WithLogger)

import NTP.Packet (NtpPacket (..), mkCliNtpPacket)
import NTP.Util   (datagramPacketSize, resolveNtpHost)

type StopWorker m = m ()

data NtpClientSettings = NtpClientSettings
    { ntpServers :: [String]
    , ntpHandler :: forall m . ( MonadIO m, WithLogger m )
                 => Microsecond -> m ()
    , ntpLogName :: String
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
        , ntpHandler = \time -> liftIO . putStrLn $ "Got time: " ++ show time
        , ntpLogName = "ntp-cli"
        }

newtype NtpStopButton = NtpStopButton
    { press :: forall m . ( MonadIO m ) => m ()
    }

newtype FailedToResolveHost = FailedToResolveHost String
    deriving (Show, Typeable)

instance Exception FailedToResolveHost


log :: WithLogger m => NtpClient -> String -> m ()
log cli msg = do
    closed <- liftIO $ readTVarIO (ncClosed cli)
    unless closed $ do
        let logName = ntpLogName (ncSettings cli)
        putStrLn $ logName ++ ": " ++ msg

doSend :: ( MonadIO m, WithLogger m )
            => SockAddr -> NtpClient -> m ()
doSend addr cli = do
    sock <- liftIO $ readTVarIO (ncSocket cli)
    let packet = encode mkCliNtpPacket
    liftIO $ sendManyTo sock (LBS.toChunks packet) addr `catchAll` handleE
  where
    -- just log; socket closure is handled by receiver
    handleE e = do
        log cli $ "Failed to send to " ++ show addr

startSend :: ( MonadIO m, WithLogger m )
          => [SockAddr] -> NtpClient -> m ()
startSend addrs cli = forever $ do
    closed <- liftIO $ readTVarIO (ncClosed cli)
    unless closed $ do
        forM_ addrs $ \addr -> forkIO $ sendRequest addr cli
        liftIO $ threadDelay 1000000

mkSocket :: MonadIO m => m Socket
mkSocket = liftIO $ do
    sock <- socket AF_INET Datagram defaultProtocol
    setSocketOption sock ReuseAddr 1
    return sock

doReceive :: ( MonadIO m, WithLogger m ) => NtpClient -> m ()
doReceive cli = do
    sock <- readTVarIO $ ncSocket cli
    let handler = ntpHandler (ncSettings cli)
    forever $ do
        (received, _) <- liftIO $ recvFrom sock datagramPacketSize
        let eNtpPacket = decodeOrFail $ LBS.fromStrict received
        case eNtpPacket of
            Left  (_, _, err)    -> log cli $ "Error while receiving time: " ++ show err
            Right (_, _, packet) -> handler $ ntpTime packet

startReceive :: ( MonadIO m, WithLogger m)
             => NtpClient -> m ()
startReceive cli = do
    receiveReplies cli `catchAll` handleE
  where
    -- got error while receiving data, recreate socket
    handleE e = do
        closed <- readTVarIO (ncClosed cli)
        unless closed $ do
            log cli $ "Socket closed, recreating (" ++ show e ++ ")"
            sock <- mkSocket
            closed' <- atomically $ do
                writeTVar (ncSocket cli) sock
                readTVar  (ncClosed cli)
            -- extra check in case socket was closed by stopping client
            -- while we recreated socket
            unless closed $
                startReceive cli

stopNtpClient :: MonadIO m => NtpClient -> m ()
stopNtpClient cli = liftIO $ do
    sock <- liftIO . atomically $ do
        writeTVar (ncClosed cli) True
        readTVar  (ncSocket cli)

    -- unblock receiving from socket in case no one replies
    liftIO $ close sock `catchAll` \_ -> return ()

startNtpClient :: ( MonadIO m, WithLogger m )
               => NtpClientSettings -> m NtpStopButton
startNtpClient settings = do
    sock <- mkSocket
    cli <- mkNtpClient settings sock

    forkIO $ startReceive cli

    addrs <- mapM resolveHost $ ntpServers settings
    void . forkIO $ startSend addrs cli

    return $ NtpStopButton { press = stopNtpClient cli }
  where
    resolveHost host = do
        maddr <- liftIO $ resolveNtpHost host
        case maddr of
            Nothing   -> throwM $ FailedToResolveHost host
            Just addr -> return addr
