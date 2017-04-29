{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This module implements functionality of NTP client.

module NTP.Client
    ( startNtpClient
    , NtpClientSettings (..)
    , NtpStopButton (..)
    , ntpSingleShot
    , hoistNtpClientSettings
    ) where

import           Control.Concurrent.STM      (atomically, modifyTVar')
import           Control.Concurrent.STM.TVar (TVar, newTVarIO, readTVar, readTVarIO,
                                              writeTVar)
import           Control.Lens                ((%=), (.=), _Just)
import           Control.Monad               (forM_, forever, unless, void, when)
import           Control.Monad.Catch         (Exception)
import           Control.Monad.Catch         (bracketOnError)
import           Control.Monad.State         (gets)
import           Control.Monad.Trans         (MonadIO (..))
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Data.Binary                 (decodeOrFail, encode)
import qualified Data.ByteString.Lazy        as LBS
import           Data.Default                (Default (..))
import           Data.List                   (sortOn)
import           Data.Maybe                  (catMaybes)
import           Data.Maybe                  (isNothing)
import           Data.Monoid                 ((<>))
import           Data.Text                   (Text)
import           Data.Time.Units             (Microsecond, Second, toMicroseconds)
import           Data.Typeable               (Typeable)
import           Formatting                  (sformat, shown, (%))
import           Network.Socket              (AddrInfo, SockAddr (..), Socket,
                                              addrAddress, addrFamily, close)
import           Network.Socket.ByteString   (recvFrom, sendTo)
import           Prelude                     hiding (log)
import           Serokell.Util.Concurrent    (modifyTVarS, threadDelay)
import           System.Wlog                 (LoggerName, Severity (..), WithLogger,
                                              logMessage, modifyLoggerName)
import           Universum                   (MonadMask, whenJust)

import           Mockable.Class              (Mockable)
import           Mockable.Concurrent         (Delay, Fork, delay, fork)
import           Mockable.Exception          (Catch, Throw, catchAll, handleAll, throw)
import           NTP.Packet                  (NtpPacket (..), evalClockOffset,
                                              mkCliNtpPacket, ntpPacketSize)
import           NTP.Util                    (createAndBindSock, resolveNtpHost,
                                              selectIPv4, selectIPv6, udpLocalAddresses,
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
    { ncSockets  :: TVar Sockets
    , ncState    :: TVar (Maybe [(Microsecond, Microsecond)])
    , ncClosed   :: TVar Bool
    , ncSettings :: NtpClientSettings m
    }

mkNtpClient :: MonadIO m => NtpClientSettings m -> Sockets -> m (NtpClient m)
mkNtpClient ncSettings sock = liftIO $ do
    ncSockets <- newTVarIO sock
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

data NoHostResolved = NoHostResolved
    deriving (Show, Typeable)

instance Exception NoHostResolved

type NtpMonad m =
    ( MonadIO m
    , MonadBaseControl IO m
    , WithLogger m
    , Mockable Fork m
    , Mockable Throw m
    , Mockable Catch m
    , MonadMask m
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
    sock   <- liftIO $ readTVarIO $ ncSockets cli
    packet <- encode <$> mkCliNtpPacket
    handleAll handleE . void . liftIO $ sendDo addr sock (LBS.toStrict packet)
  where
    sendDo a@(SockAddrInet _ _) (IPv4Sock sock)      = sendTo' sock a
    sendDo a@(SockAddrInet _ _) (BothSock sock _)    = sendTo' sock a
    sendDo a@(SockAddrInet6 _ _ _ _) (IPv6Sock sock) = sendTo' sock a
    sendDo a@(SockAddrInet6 _ _ _ _) (BothSock _ sock)  = sendTo' sock a
    sendDo a sks                                           =
        error $ "SockAddr is " ++ show a ++ ", but sockets: " ++ show sks
    sendTo' sock = flip (sendTo sock)

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

-- Try to create IPv4 and IPv6 socket.
mkSockets :: forall m . NtpMonad m => NtpClientSettings m -> m Sockets
mkSockets settings = do
    (sock1MB, sock2MB) <- doMkSockets `catchAll` handlerE
    whenJust sock1MB logging
    whenJust sock2MB logging
    case (fst <$> sock1MB, fst <$> sock2MB) of
        (Just sock1, Just sock2) -> pure $ BothSock sock1 sock2
        (Just sock1, Nothing)    -> pure $ IPv4Sock sock1
        (Nothing, Just sock2)    -> pure $ IPv6Sock sock2
        (_, _)                   -> do
            log' settings Warning "Couldn't create both IPv4 and IPv6 socket, retrying in 5 sec..."
            liftIO $ threadDelay (5 :: Second)
            mkSockets settings
  where
    logging (_, addrInfo) = log' settings Info $
        sformat ("Created socket (family/addr): "%shown%"/"%shown)
                (addrFamily addrInfo) (addrAddress addrInfo)
    doMkSockets :: m (Maybe (Socket, AddrInfo), Maybe (Socket, AddrInfo))
    doMkSockets = liftIO $ do
        serveraddrs <- udpLocalAddresses
        (,) <$> createAndBindSock selectIPv4 serveraddrs
            <*> createAndBindSock selectIPv6 serveraddrs
    handlerE e = do
        log' settings Warning $
            sformat ("Failed to create sockets, retrying in 5 sec... (reason: "%shown%")")
            e
        liftIO $ threadDelay (5 :: Second)
        doMkSockets

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
            handleNtpPacket cli packet `catchAll` handleE
  where
    handleE = log cli Warning . sformat ("Error while handle packet: "%shown)

startReceive :: NtpMonad m => NtpClient m -> m ()
startReceive cli = do
    sockets <- liftIO . atomically . readTVar $ ncSockets cli
    case sockets of
        BothSock sIPv4 sIPv6 -> do
            void $ fork $ runDoReceive True sIPv4
            runDoReceive False sIPv6
        IPv4Sock sIPv4 -> runDoReceive True sIPv4
        IPv6Sock sIPv6 -> runDoReceive False sIPv6
  where
    runDoReceive isIPv4 sock = doReceive sock cli `catchAll` handleE isIPv4 sock
    -- got error while receiving data, retrying in 5 sec
    handleE isIPv4 sock e = do
        closed <- liftIO . readTVarIO $ ncClosed cli
        unless closed $ do
            log cli Debug $ sformat ("doReceive failed on socket"%shown%
                                     ", reason: "%shown%
                                     ", recreate socket in 5 sec") sock e
            liftIO $ threadDelay (5 :: Second)
            serveraddrs <- liftIO udpLocalAddresses
            newSockMB <- liftIO $
                if isIPv4 then
                    traverse (overwriteSocket IPv4Sock . fst) =<< createAndBindSock selectIPv4 serveraddrs
                else
                    traverse (overwriteSocket IPv6Sock . fst) =<< createAndBindSock selectIPv6 serveraddrs
            case newSockMB of
                Nothing      -> log cli Warning "Recreating of socket failed" >> handleE isIPv4 sock e
                Just newSock -> runDoReceive isIPv4 newSock
    overwriteSocket constr sock = sock <$
        (liftIO .
         atomically .
         modifyTVar' (ncSockets cli) .
         flip mergeSockets .
         constr $ sock)

stopNtpClient :: NtpMonad m => NtpClient m -> m ()
stopNtpClient cli = do
    log cli Info "Stopped"
    sockets <- liftIO . atomically $ do
        writeTVar (ncClosed cli) True
        socketsToList <$> readTVar (ncSockets cli)

    -- unblock receiving from socket in case no one replies
    forM_ sockets $ \s -> liftIO (close s) `catchAll` (const $ pure ())

startNtpClient :: NtpMonad m => NtpClientSettings m -> m (NtpStopButton m)
startNtpClient settings =
    withSocketsDoLifted $
    bracketOnError (mkSockets settings) closeSockets $ \sock -> do
        cli <- mkNtpClient settings sock

        addrs <- catMaybes <$> mapM (resolveHost cli $ socketsToBoolDescr sock)
                                    (ntpServers settings)
        if null addrs then
            throw NoHostResolved
        else do
            void . fork . withSocketsDoLifted $ startReceive cli
            void . fork . withSocketsDoLifted $ startSend addrs cli
            log cli Info "Launched"

        return $ NtpStopButton $ stopNtpClient cli
  where
    closeSockets sockets = forM_ (socketsToList sockets) (liftIO . close)
    resolveHost cli sockDescr host = do
        maddr <- liftIO $ resolveNtpHost host sockDescr
        case maddr of
            Nothing   -> do
                log cli Warning $ sformat ("Host "%shown%" is not resolved") host
                pure Nothing
            Just addr -> do
                log cli Info $ sformat ("Host "%shown%" is resolved: "%shown) host addr
                pure $ Just addr

-- | Start client, wait for a while so that most likely it ticks once
-- and stop it.
ntpSingleShot
    :: (Mockable Delay m, NtpMonad m)
    => NtpClientSettings m -> m ()
ntpSingleShot settings = do
    stopButton <- startNtpClient settings
    delay (ntpResponseTimeout settings)
    pressNtpStopButton stopButton

-- Store created sockets.
-- If system supports IPv6 and IPv4 we create socket for IPv4 and IPv6.
-- Otherwise only one.
data Sockets
    = IPv4Sock !Socket
    | IPv6Sock !Socket
    | BothSock !Socket !Socket
    deriving Show

socketsToList :: Sockets -> [Socket]
socketsToList (BothSock s1 s2) = [s1, s2]
socketsToList (IPv4Sock s1)    = [s1]
socketsToList (IPv6Sock s1)    = [s1]

socketsToBoolDescr :: Sockets -> (Bool, Bool)
socketsToBoolDescr (BothSock _ _) = (True, True)
socketsToBoolDescr (IPv4Sock _)   = (True, False)
socketsToBoolDescr (IPv6Sock _)   = (False, True)

--              Old        New
mergeSockets :: Sockets -> Sockets -> Sockets
mergeSockets (BothSock _ v6) (IPv4Sock s) = BothSock s v6
mergeSockets (BothSock v4 _) (IPv6Sock s) = BothSock v4 s
mergeSockets (IPv6Sock _) (IPv6Sock s)    = IPv6Sock s
mergeSockets (IPv4Sock _) (IPv4Sock s)    = IPv4Sock s
mergeSockets _ _                          = error "Unexpected state of mergeSockets"
