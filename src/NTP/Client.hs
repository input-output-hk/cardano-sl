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

import NTP.Packet (NtpPacket (..), mkCliNtpPacket)
import NTP.Util   (datagramPacketSize, resolveNtpHost)

type StopWorker m = m ()

data NtpClientSettings = NtpClientSettings
    { ntpServers :: [String]
    , ntpHandler :: forall m . ( MonadIO m )
                 => Microsecond -> m ()
    , ntpLogName :: String
    }

data NtpClient = NtpClient
    { ncSocket   :: TVar Socket
    , ncClosed   :: TVar Bool
    , ncSettings :: NtpClientSettings
    }

mkNtpClient :: NtpClientSettings -> Socket -> IO NtpClient
mkNtpClient ncSettings sock = do
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


log :: NtpClient -> String -> IO ()
log cli msg = do
    closed <- readTVarIO (ncClosed cli)
    unless closed $ do
        let logName = ntpLogName (ncSettings cli)
        putStrLn $ logName ++ ": " ++ msg

sendRequest :: SockAddr -> NtpClient -> IO ()
sendRequest addr cli = do
    sock <- readTVarIO (ncSocket cli)
    let packet = encode mkCliNtpPacket
    sendManyTo sock (LBS.toChunks packet) addr `catchAll` handleE
  where
    -- just log; socket closure is handled by receiver
    handleE e = do
        log cli $ "Failed to send to " ++ show addr

startSend :: [SockAddr] -> NtpClient -> IO ()
startSend addrs cli = forever $ do
    closed <- readTVarIO (ncClosed cli)
    unless closed $ do
        forM_ addrs $ \addr -> forkIO $ sendRequest addr cli
        threadDelay 1000000

mkSocket :: IO Socket
mkSocket = do
    sock <- socket AF_INET Datagram defaultProtocol
    setSocketOption sock ReuseAddr 1
    return sock

receiveReplies :: NtpClient -> IO ()
receiveReplies cli = do
    sock <- readTVarIO $ ncSocket cli
    let handler = ntpHandler (ncSettings cli)
    forever $ do
        (received, _) <- recvFrom sock datagramPacketSize
        let eNtpPacket = decodeOrFail $ LBS.fromStrict received
        case eNtpPacket of
            Left  (_, _, err)    -> log cli $ "Error while receiving time: " ++ show err
            Right (_, _, packet) -> handler $ ntpTime packet

startReceive :: NtpClient -> IO ()
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
    sock <- atomically $ do
        writeTVar (ncClosed cli) True
        readTVar  (ncSocket cli)

    -- unblock receiving from socket in case no one replies
    close sock `catchAll` \_ -> return ()

startNtpClient :: NtpClientSettings -> IO NtpStopButton
startNtpClient settings = do
    sock <- mkSocket
    cli <- mkNtpClient settings sock

    forkIO $ startReceive cli

    addrs <- mapM resolveHost $ ntpServers settings
    void . forkIO $ startSend addrs cli

    return $ NtpStopButton { press = stopNtpClient cli }
  where
    resolveHost host = do
        maddr <- resolveNtpHost host
        case maddr of
            Nothing   -> throwM $ FailedToResolveHost host
            Just addr -> return addr
