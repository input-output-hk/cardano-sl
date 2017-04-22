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
    , ntpSingleShot
    , hoistNtpClientSettings
    ) where

import           Control.Concurrent.STM      (atomically)
import           Control.Concurrent.STM.TVar (TVar, newTVarIO, readTVar, readTVarIO,
                                              writeTVar)
import           Control.Lens                ((%=), (.=), _Just)
import           Control.Monad               (forM_, forever, unless, void, when)
import           Control.Monad.Catch         (Exception)
import           Control.Monad.State         (gets)
import           Control.Monad.Trans         (MonadIO (..))
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Data.Binary                 (decodeOrFail, encode)
import qualified Data.ByteString.Lazy        as LBS
import           Data.Default                (Default (..))
import           Data.List                   (sortOn)
import           Data.Maybe                  (isJust, isNothing)
import           Data.Monoid                 ((<>))
import           Data.Text                   (Text)
import           Data.Time.Units             (Microsecond, Second, toMicroseconds)
import           Data.Typeable               (Typeable)
import           Formatting                  (sformat, shown, (%))
import           Network.Socket              (AddrInfoFlag (AI_PASSIVE), SockAddr (..),
                                              Socket, SocketOption (ReuseAddr),
                                              SocketType (Datagram), aNY_PORT,
                                              addrAddress, addrFamily, addrFlags,
                                              addrSocketType, bind, close, defaultHints,
                                              defaultProtocol, getAddrInfo,
                                              setSocketOption, socket)
import           Network.Socket.ByteString   (recvFrom, sendTo)
import           Prelude                     hiding (log)
import           Serokell.Util.Concurrent    (modifyTVarS, threadDelay)
import           System.Wlog                 (LoggerName, Severity (..), WithLogger,
                                              logMessage, modifyLoggerName)

import           Mockable.Class              (Mockable)
import           Mockable.Concurrent         (Delay, Fork, delay, fork)
import           Mockable.Exception          (Catch, Throw, catchAll, handleAll, throw)
import           NTP.Packet                  (NtpPacket (..), evalClockOffset,
                                              mkCliNtpPacket, ntpPacketSize)
import           NTP.Util                    (resolveNtpHost, selectIPv4, selectIPv6,
                                              withSocketsDoLifted)

data NtpClientSettings m = NtpClientSettings
    { ntpServers         :: [String]
      -- ^ list of servers addresses
    , ntpHandler         :: (Microsecond, Microsecond) -> m ()
      -- ^ got time callback (margin, time when client sent request)
    , ntpLogName         :: LoggerName
      -- ^ logger name modifier
    , ntpResponseTimeout :: Microsecond
      -- ^ delay between making requests and response collection;
      -- it also means that handler will be invoked with this lag
    , ntpPollDelay       :: Microsecond
      -- ^ how often to send responses to server
    , ntpMeanSelection   :: [(Microsecond, Microsecond)] -> (Microsecond, Microsecond)
      -- ^ way to sumarize results received from different servers.
      -- this may accept list of lesser size than @length ntpServers@ in case some servers
      -- failed to respond in time, but never an empty list
    }

hoistNtpClientSettings
    :: (m () -> n ()) -> NtpClientSettings m -> NtpClientSettings n
hoistNtpClientSettings f settings =
    settings { ntpHandler = f . ntpHandler settings }

data NtpClient m = NtpClient
    { ncSocket   :: TVar (Maybe Socket, Maybe Socket)
    , ncState    :: TVar (Maybe [(Microsecond, Microsecond)])
    , ncClosed   :: TVar Bool
    , ncSettings :: NtpClientSettings m
    }

mkNtpClient :: MonadIO m => NtpClientSettings m -> (Maybe Socket, Maybe Socket) -> m (NtpClient m)
mkNtpClient ncSettings sockMB = liftIO $ case sockMB of
    (Nothing, Nothing) -> error "Both sockets are invalid"
    sock -> do
        ncSocket <- newTVarIO sock
        ncState  <- newTVarIO Nothing
        ncClosed <- newTVarIO False
        return NtpClient{..}

instance Monad m => Default (NtpClientSettings m) where
    def = NtpClientSettings
        { ntpServers         = [ "ntp5.stratum2.ru"
                               , "ntp1.stratum1.ru"
                               , "clock.isc.org"
                               ]
        , ntpHandler         = \_ -> return ()
        , ntpLogName         = "ntp-cli"
        , ntpResponseTimeout = 1000000
        , ntpPollDelay       = 3000000
        , ntpMeanSelection   = \l -> let len = length l in (sortOn fst l) !! ((len - 1) `div` 2)
        }

newtype NtpStopButton m = NtpStopButton
    { pressNtpStopButton :: m ()
    }

newtype FailedToResolveHost = FailedToResolveHost String
    deriving (Show, Typeable)

instance Exception FailedToResolveHost

type NtpMonad m =
    ( MonadIO m
    , MonadBaseControl IO m
    , WithLogger m
    , Mockable Fork m
    , Mockable Throw m
    , Mockable Catch m
    )

log' :: NtpMonad m => NtpClientSettings m -> Severity -> Text -> m ()
log' settings severity msg =
    let logName = ntpLogName settings
    in  modifyLoggerName (<> logName) $ logMessage severity msg

log :: NtpMonad m => NtpClient m -> Severity -> Text -> m ()
log cli severity msg = do
    closed <- liftIO $ readTVarIO (ncClosed cli)
    unless closed $ log' (ncSettings cli) severity msg

handleCollectedResponses :: NtpMonad m => NtpClient m -> m ()
handleCollectedResponses cli = do
    mres <- liftIO $ readTVarIO (ncState cli)
    let selection = ntpMeanSelection (ncSettings cli)
        handler   = ntpHandler (ncSettings cli)
    case mres of
        Nothing        -> log cli Error "Protocol error: responses are not awaited"
        Just []        -> log cli Warning "No servers responded"
        Just responses -> handleE `handleAll` do
            let time = selection responses
            log cli Info $ sformat ("Evaluated clock offset "%shown%
                " mcs for request at "%shown%" mcs")
                (toMicroseconds $ fst time)
                (toMicroseconds $ snd time)
            handler time
  where
    handleE = log cli Error . sformat ("ntpMeanSelection: "%shown)

doSend :: NtpMonad m => SockAddr -> NtpClient m -> m ()
doSend addr cli = do
    sock   <- liftIO $ readTVarIO (ncSocket cli)
    packet <- encode <$> mkCliNtpPacket
    handleAll handleE . void . liftIO $ sendDo addr (LBS.toStrict packet) sock
  where
    sendDo a@(SockAddrInet _ _) bytes (Just sock, _)      = sendTo sock bytes a
    sendDo a@(SockAddrInet6 _ _ _ _) bytes (_, Just sock) = sendTo sock bytes a
    sendDo _ _ _                                          = error "Unexpected SockAddr"
    -- just log; socket closure is handled by receiver
    handleE =
        log cli Warning . sformat ("Failed to send to "%shown%": "%shown) addr

startSend :: NtpMonad m => [SockAddr] -> NtpClient m -> m ()
startSend addrs cli = do
    let timeout = ntpResponseTimeout (ncSettings cli)
    let poll    = ntpPollDelay (ncSettings cli)
    closed <- liftIO $ readTVarIO (ncClosed cli)
    unless closed $ do
        log cli Debug "Sending requests"
        liftIO . atomically . modifyTVarS (ncState cli) $ id .= Just []
        forM_ addrs $
            \addr -> fork $ doSend addr cli
        liftIO $ threadDelay timeout

        log cli Debug "Collecting responses"
        handleCollectedResponses cli
        liftIO . atomically . modifyTVarS (ncState cli) $ id .= Nothing
        liftIO $ threadDelay (poll - timeout)

        startSend addrs cli

whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust val f = maybe (pure ()) f val

-- Try to create IPv4 and IPv6 socket.
mkSockets :: NtpMonad m => NtpClientSettings m -> m (Maybe Socket, Maybe Socket)
mkSockets settings = do
    (sock1, sock2) <- doMkSocket `catchAll` handlerE
    whenJust sock1 logging
    whenJust sock2 logging
    pure (fst <$> sock1, fst <$> sock2)
  where
    logging (_, addrInfo) = log' settings Info $
        sformat ("Created socket (family/addr): "%shown%"/"%shown)
                (addrFamily addrInfo) (addrAddress addrInfo)
    createSock selecter serveraddrs = case selecter serveraddrs of
        Nothing -> undefined
        Just serveraddr -> do
            sock <- socket (addrFamily serveraddr) Datagram defaultProtocol
            setSocketOption sock ReuseAddr 1
            bind sock (addrAddress serveraddr)
            pure $ Just (sock, serveraddr)
    doMkSocket = liftIO $ do
        let hints = defaultHints { addrFlags = [AI_PASSIVE], addrSocketType = Datagram }
        --                          Hints        Host         Service
        serveraddrs <- getAddrInfo (Just hints) Nothing (Just $ show aNY_PORT)
        (,) <$> createSock selectIPv4 serveraddrs
            <*> createSock selectIPv6 serveraddrs
    handlerE e = do
        log' settings Warning $
            sformat ("Failed to create socket, retrying in 5 sec... (reason: "%shown%")")
            e
        liftIO $ threadDelay (5 :: Second)
        doMkSocket

handleNtpPacket :: NtpMonad m => NtpClient m -> NtpPacket -> m ()
handleNtpPacket cli packet = do
    log cli Debug $ sformat ("Got packet "%shown) packet

    clockOffset <- evalClockOffset packet

    log cli Debug $ sformat ("Received time delta "%shown%" mcs")
        (toMicroseconds clockOffset)

    late <- liftIO . atomically . modifyTVarS (ncState cli) $ do
        _Just %= ((clockOffset, ntpOriginTime packet) :)
        gets isNothing
    when late $
        log cli Warning "Response was too late"

doReceive :: NtpMonad m => Socket -> NtpClient m -> m ()
doReceive sock cli = forever $ do
        (received, _) <- liftIO $ recvFrom sock ntpPacketSize
        let eNtpPacket = decodeOrFail $ LBS.fromStrict received
        case eNtpPacket of
            Left  (_, _, err)    ->
                log cli Warning $ sformat ("Error while receiving time: "%shown) err
            Right (_, _, packet) ->
                handleNtpPacket cli packet

startReceive :: NtpMonad m => NtpClient m -> m ()
startReceive cli = do
    sockets <- liftIO . atomically . readTVar $ (ncSocket cli)
    case sockets of
        (Just sock1, Just sock2) -> do
            void $ fork $ doReceive sock1 cli `catchAll` handleE
            doReceive sock2 cli `catchAll` handleE
        (Just sock1, _) -> doReceive sock1 cli `catchAll` handleE
        (_, Just sock2) -> doReceive sock2 cli `catchAll` handleE
        _ -> log cli Warning $ "Unexpected state to start"
  where
    -- got error while receiving data, recreate socket
    handleE e = do
        closed <- liftIO . readTVarIO $ ncClosed cli
        unless closed $ do
            log cli Debug $ sformat ("Socket closed, recreating (reason: "%shown%")") e
            sock <- mkSockets $ ncSettings cli
            closed' <- liftIO . atomically $ do
                writeTVar (ncSocket cli) sock
                readTVar  (ncClosed cli)
            -- extra check in case socket was closed by stopping client
            -- while we recreated socket
            unless closed' $
                startReceive cli

stopNtpClient :: NtpMonad m => NtpClient m -> m ()
stopNtpClient cli = do
    log cli Info "Stopped"
    (sock1, sock2) <- liftIO . atomically $ do
        writeTVar (ncClosed cli) True
        readTVar  (ncSocket cli)

    -- unblock receiving from socket in case no one replies
    whenJust sock1 $ \s -> liftIO (close s) `catchAll` (\_ -> pure ())
    whenJust sock2 $ \s -> liftIO (close s) `catchAll` (\_ -> pure ())

startNtpClient :: NtpMonad m => NtpClientSettings m -> m (NtpStopButton m)
startNtpClient settings = do
    sock <- mkSockets settings
    cli <- mkNtpClient settings sock

    void . fork . withSocketsDoLifted $ startReceive cli

    addrs <- mapM (resolveHost cli sock) (ntpServers settings)
    void . fork . withSocketsDoLifted $ startSend addrs cli

    log cli Info "Launched"

    return $ NtpStopButton $ stopNtpClient cli
  where
    resolveHost cli (s1, s2) host = do
        maddr <- liftIO $ resolveNtpHost host (isJust s1, isJust s2)
        case maddr of
            Nothing   -> throw $ FailedToResolveHost host
            Just addr -> do
                log cli Info $ sformat ("Host "%shown%" is resolved: "%shown) host addr
                return addr

-- | Start client, wait for a while so that most likely it ticks once
-- and stop it.
ntpSingleShot
    :: (Mockable Delay m, NtpMonad m)
    => NtpClientSettings m -> m ()
ntpSingleShot settings = do
    stopButton <- startNtpClient settings
    delay (ntpResponseTimeout settings)
    pressNtpStopButton stopButton
