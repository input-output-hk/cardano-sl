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
import           Control.Concurrent.STM      (atomically)
import           Control.Concurrent.STM.TVar (TVar, newTVarIO, readTVar, readTVarIO,
                                              writeTVar)
import           Control.Lens                (hasn't, to, use, uses, (%=), (.=), (^?),
                                              _Just, _head)
import           Control.Monad               (forM_, forever, unless, void, when)
import           Control.Monad.Catch         (Exception, SomeException (..))
import           Control.Monad.Trans         (MonadIO (..))
import           Data.Binary                 (decodeOrFail, encode)
import qualified Data.ByteString.Lazy        as LBS
import           Data.Default                (Default (..))
import           Data.List                   (sort)
import qualified Data.Map                    as M
import           Data.Monoid                 ((<>))
import           Data.Text                   (Text)
import           Data.Time.Units             (Microsecond, Second)
import           Data.Time.Units             (fromMicroseconds)
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
import           Serokell.Util.Concurrent    (modifyTVarS, threadDelay)
import           System.Wlog                 (LoggerName, Severity (..), WithLogger,
                                              logMessage, modifyLoggerName,
                                              usingLoggerName)

import Mockable.Class      (Mockable)
import Mockable.Concurrent (Fork, fork)
import Mockable.Exception  (Catch, Throw, catch, catchAll, handleAll, throw)
import Mockable.Production (runProduction)
import NTP.Packet          (NtpPacket (..), evalClockOffset, mkCliNtpPacket)
import NTP.Util            (datagramPacketSize, getCurrentTime, resolveNtpHost)

type StopWorker m = m ()

data NtpClientSettings = NtpClientSettings
    { ntpServers         :: [String]
      -- ^ list of servers addresses
    , ntpHandler         :: forall m . ( MonadIO m, WithLogger m )
                         => Microsecond -> m ()
      -- ^ got time callback
    , ntpLogName         :: LoggerName
      -- ^ logger name modifier
    , ntpResponseTimeout :: Microsecond
      -- ^ delay between making requests and response collection;
      -- it also means that handler will be invoked with this lag
    , ntpPollDelay       :: Microsecond
      -- ^ how often to send responses to server
    , ntpMeanSelection   :: [Microsecond] -> Microsecond
      -- ^ way to sumarize results received from different servers.
      -- this may accept list of lesser size than @length ntpServers@ in case some servers
      -- failed to respond in time
    }

data NtpClient = NtpClient
    { ncSocket   :: TVar Socket
    , ncState    :: TVar (Maybe [Microsecond])
    , ncClosed   :: TVar Bool
    , ncSettings :: NtpClientSettings
    }

mkNtpClient :: MonadIO m => NtpClientSettings -> Socket -> m NtpClient
mkNtpClient ncSettings sock = liftIO $ do
    ncSocket <- newTVarIO sock
    ncState  <- newTVarIO Nothing
    ncClosed <- newTVarIO False
    return NtpClient{..}

instance Default NtpClientSettings where
    def = NtpClientSettings
        { ntpServers         = [ "ntp5.stratum2.ru"
                               , "ntp1.stratum1.ru"
                               , "clock.isc.org"
                               ]
        , ntpHandler         = \_ -> return ()
        , ntpLogName         = "ntp-cli"
        , ntpResponseTimeout = 100000
        , ntpPollDelay       = 1000000
        , ntpMeanSelection   = \l -> let len = length l in sort l !! ((len - 1) `div` 2)
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

log' :: NtpMonad m => NtpClientSettings -> Severity -> Text -> m ()
log' settings severity msg =
    let logName = ntpLogName settings
    in  modifyLoggerName (<> logName) $ logMessage severity msg

log :: NtpMonad m => NtpClient -> Severity -> Text -> m ()
log cli severity msg = do
    closed <- liftIO $ readTVarIO (ncClosed cli)
    unless closed $ log' (ncSettings cli) severity msg

handleCollectedResponses :: NtpMonad m => NtpClient -> m ()
handleCollectedResponses cli = do
    mres <- liftIO . atomically . modifyTVarS (ncState cli) $ use id
    let selection = ntpMeanSelection (ncSettings cli)
        handler   = ntpHandler (ncSettings cli)
    case mres of
        Nothing        -> log cli Error "Protocol error: responses are not awaited"
        Just []        -> log cli Warning "No servers responded"
        Just responses -> handleE `handleAll` do
            let time = selection responses
            log cli Info $ sformat ("Evaluated time: "%shown) time
            handler time
  where
    handleE = log cli Error . sformat ("ntpMeanSelection: "%shown)

doSend :: NtpMonad m => SockAddr -> NtpClient -> m ()
doSend addr cli = do
    sock   <- liftIO $ readTVarIO (ncSocket cli)
    packet <- encode <$> mkCliNtpPacket
    liftIO (sendManyTo sock (LBS.toChunks packet) addr) `catchAll` handleE
  where
    -- just log; socket closure is handled by receiver
    handleE e =
        log cli Warning $ sformat ("Failed to send to "%shown) addr

startSend :: NtpMonad m => [SockAddr] -> NtpClient -> m ()
startSend addrs cli = forever $ do
    let timeout = ntpResponseTimeout (ncSettings cli)
    let poll    = ntpPollDelay (ncSettings cli)
    closed <- liftIO $ readTVarIO (ncClosed cli)
    unless closed $ do
        liftIO . atomically . modifyTVarS (ncState cli) $ id .= Just []
        forM_ addrs $
            \addr -> fork $ doSend addr cli
        liftIO $ threadDelay timeout

        handleCollectedResponses cli
        liftIO . atomically . modifyTVarS (ncState cli) $ id .= Nothing
        liftIO $ threadDelay (poll - timeout)

mkSocket :: NtpMonad m => NtpClientSettings -> m Socket
mkSocket settings = doMkSocket `catchAll` handlerE settings
  where
    doMkSocket = liftIO $ do
        sock <- socket AF_INET Datagram defaultProtocol
        setSocketOption sock ReuseAddr 1
        return sock
    handlerE cli e = do
        log' settings Warning $
            sformat ("Failed to create socket, retrying in 5 sec... (reason: "%shown%")")
            e
        liftIO $ threadDelay (5 :: Second)
        mkSocket settings

handleNtpPacket :: NtpMonad m => NtpClient -> NtpPacket -> m ()
handleNtpPacket cli packet = do
    log cli Debug $ sformat ("Got packet "%shown) packet

    clockOffset <- evalClockOffset packet

    log cli Info $ sformat ("Received time delta "%shown) clockOffset

    late <- liftIO . atomically . modifyTVarS (ncState cli) $ do
        _Just %= (clockOffset :)
        uses id $ hasn't _Just
    when late $
        log cli Warning "Note, previous response was too late"

doReceive :: NtpMonad m => NtpClient -> m ()
doReceive cli = do
    sock <- liftIO . readTVarIO $ ncSocket cli
    forever $ do
        (received, _) <- liftIO $ recvFrom sock datagramPacketSize
        let eNtpPacket = decodeOrFail $ LBS.fromStrict received
        case eNtpPacket of
            Left  (_, _, err)    ->
                log cli Warning $ sformat ("Error while receiving time: "%shown) err
            Right (_, _, packet) ->
                handleNtpPacket cli packet

startReceive :: NtpMonad m => NtpClient -> m ()
startReceive cli = do
    doReceive cli `catchAll` handleE
  where
    -- got error while receiving data, recreate socket
    handleE e = do
        closed <- liftIO . readTVarIO $ ncClosed cli
        unless closed $ do
            log cli Debug $ sformat ("Socket closed, recreating (reason: "%shown%")") e
            sock <- mkSocket $ ncSettings cli
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
    sock <- mkSocket settings
    cli <- mkNtpClient settings sock

    fork $ startReceive cli

    addrs <- mapM resolveHost $ ntpServers settings
    void . fork $ startSend addrs cli

    log cli Info "Launched"

    return NtpStopButton { press = stopNtpClient cli }
  where
    resolveHost host = do
        maddr <- liftIO $ resolveNtpHost host
        case maddr of
            Nothing   -> throw $ FailedToResolveHost host
            Just addr -> return addr
