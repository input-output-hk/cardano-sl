{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This module implements functionality of NTP client.

module Ntp.Client
    ( NtpClientSettings (..)
    , NtpStatus (..)
    , withNtpClient
    , withoutNtpClient
    , ntpSingleShot
    ) where

import           Universum

import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Async (withAsync, async, concurrently, forConcurrently, race)
import           Control.Concurrent.STM (TVar, check, modifyTVar')
import           Control.Exception.Safe (Exception, catchAny, handleAny)
import           Control.Lens ((%=), (.=), _Just)
import           Control.Monad (forever)
import           Control.Monad.State (gets)
import           Data.Binary (decodeOrFail, encode)
import qualified Data.ByteString.Lazy as LBS
import           Data.Maybe (catMaybes, isNothing)
import           Data.Time.Units (TimeUnit, Microsecond, toMicroseconds)
import           Data.Typeable (Typeable)
import           Formatting (sformat, shown, (%))
import           Network.Socket (AddrInfo, SockAddr (..), Socket, addrAddress, addrFamily, close)
import           Network.Socket.ByteString (recvFrom, sendTo)
import           Serokell.Util.Concurrent (modifyTVarS)
import           System.Wlog (LoggerNameBox)
import qualified System.Wlog as Wlog

import           Mockable (realTime)
import           Ntp.Packet (NtpPacket (..), evalClockOffset, mkCliNtpPacket, ntpPacketSize)
import           Ntp.Util (createAndBindSock, resolveNtpHost, selectIPv4, selectIPv6,
                           udpLocalAddresses, withSocketsDoLifted)

data NtpStatus =
      -- | The difference between ntp time and local system time
      NtpDrift Microsecond
      -- | NTP client has send requests to the servers
    | NtpSyncPending
      -- | NTP is not available: the client has not received any respond within
      -- `ntpResponseTimeout` or NTP was not configured.
    | NtpSyncUnavailable deriving (Eq, Show)

data NtpClientSettings = NtpClientSettings
    { ntpServers         :: [String]
      -- ^ list of servers addresses
    , ntpResponseTimeout :: Microsecond
      -- ^ delay between making requests and response collection
    , ntpPollDelay       :: Microsecond
      -- ^ how long to wait between to send requests to the servers
    , ntpMeanSelection   :: [(Microsecond, Microsecond)] -> (Microsecond, Microsecond)
      -- ^ way to sumarize results received from different servers.
      -- this may accept list of lesser size than @length ntpServers@ in case
      -- some servers failed to respond in time, but never an empty list
    }

data NtpClient = NtpClient
    { ncSockets  :: TVar Sockets
      -- ^ ntp client sockets: ipv4 / ipv6 / both
    , ncState    :: TVar (Maybe [(Microsecond, Microsecond)])
      -- ^ ntp client state, list of received values
    , ncStatus          :: TVar NtpStatus
      -- ^ got time callback (margin, time when client sent request)
    , ncSettings :: NtpClientSettings
      -- ^ client configuration
    }

mkNtpClient :: MonadIO m => NtpClientSettings -> TVar NtpStatus -> Sockets -> m NtpClient
mkNtpClient ncSettings ncStatus sock = liftIO $ do
    ncSockets <- newTVarIO sock
    ncState  <- newTVarIO Nothing
    return NtpClient{..}

data NoHostResolved = NoHostResolved
    deriving (Show, Typeable)

instance Exception NoHostResolved

usingNtpLogger :: LoggerNameBox IO a -> IO a
usingNtpLogger = Wlog.usingLoggerName "NtpClient"

logError :: Text -> IO ()
logError = usingNtpLogger . Wlog.logError

logWarning :: Text -> IO ()
logWarning = usingNtpLogger . Wlog.logWarning

logInfo :: Text -> IO ()
logInfo = usingNtpLogger . Wlog.logInfo

logDebug :: Text -> IO ()
logDebug = usingNtpLogger . Wlog.logDebug

-- |
-- Handle results.  It is either when all ntp servers responded or when
-- `ntpResponseTimeout` has passed since the request where send.  If none of the servers responded `ncState`
handleCollectedResponses :: NtpClient -> IO ()
handleCollectedResponses cli = do
    mres <- readTVarIO (ncState cli)
    case mres of
        Nothing        -> do
            atomically $ writeTVar (ncStatus cli) NtpSyncUnavailable
            logError "Protocol error: responses are not awaited"
        Just []        -> do
            atomically $ writeTVar (ncStatus cli) NtpSyncUnavailable
            logWarning "No servers responded"
        Just responses -> handleE `handleAny` do
            let time = ntpMeanSelection (ncSettings cli) responses
            logInfo $ sformat ("Evaluated clock offset "%shown%
                " mcs for request at "%shown%" mcs")
                (toMicroseconds $ fst time)
                (toMicroseconds $ snd time)
            handler time
  where
    handleE = logError . sformat ("ntpMeanSelection: "%shown)

    handler :: (Microsecond, Microsecond) -> IO ()
    handler (newMargin, transmitTime) = do
        let ntpTime = transmitTime + newMargin
        localTime <- realTime
        atomically $ writeTVar (ncStatus cli) (NtpDrift $ ntpTime - localTime)


allResponsesGathered :: NtpClient -> STM Bool
allResponsesGathered cli = do
    responsesState <- readTVar $ ncState cli
    let servers = ntpServers $ ncSettings cli
    return $ case responsesState of
        Nothing        -> False
        Just responses -> length responses >= length servers

-- |
-- Low level primitive which sends a request to a single ntp server.
doSend :: NtpClient -> SockAddr -> IO ()
doSend cli addr = do
    sock   <- readTVarIO $ ncSockets cli
    packet <- encode <$> mkCliNtpPacket
    handleAny handleE . void $ sendDo addr sock (LBS.toStrict packet)
  where
    sendDo a@(SockAddrInet{}) (IPv4Sock sock)      = sendTo' sock a
    sendDo a@(SockAddrInet{}) (BothSock sock _)    = sendTo' sock a
    sendDo a@(SockAddrInet6{}) (IPv6Sock sock) = sendTo' sock a
    sendDo a@(SockAddrInet6{}) (BothSock _ sock)  = sendTo' sock a
    sendDo a sks                                           =
        error $ "SockAddr is " <> show a <> ", but sockets: " <> show sks
    sendTo' sock = flip (sendTo sock)

    -- just log; socket closure is handled by receiver
    handleE =
        logWarning . sformat ("Failed to send to "%shown%": "%shown) addr

-- |
-- Every `ntpPollDelay` send request to the list of `ntpServers`.  Before
-- sending the request, fill `ncState` with `NtpSyncPending`.  After sending
-- requests wait until either all servers respond or `ntpResponseTimeout`
-- passes.  If at least one server responded `handleCollectedResponses` will
-- update `ncStatus` in `NtpClient`.
startSend :: NtpClient -> [SockAddr] -> IO ()
startSend cli addrs = do
    let respTimeout = ntpResponseTimeout (ncSettings cli)
    let poll = ntpPollDelay (ncSettings cli)

    atomically $ writeTVar (ncStatus cli) NtpSyncPending

    -- poll :: Microsecond
    _ <- concurrently (threadDelay (fromIntegral poll)) $ do
        logDebug "Sending requests"
        atomically . modifyTVarS (ncState cli) $ identity .= Just []
        let sendRequests = forConcurrently addrs (doSend cli)
        let waitTimeout = void $ timeout respTimeout
                    (atomically $ check =<< allResponsesGathered cli)

        withAsync sendRequests $ \_ -> waitTimeout

        logDebug "Collecting responses"
        handleCollectedResponses cli
        atomically . modifyTVarS (ncState cli) $ identity .= Nothing

    startSend cli addrs

-- Try to create IPv4 and IPv6 socket.
mkSockets :: NtpClientSettings -> IO Sockets
mkSockets settings = do
    (sock1MB, sock2MB) <- doMkSockets `catchAny` handlerE
    whenJust sock1MB logging
    whenJust sock2MB logging
    case (fst <$> sock1MB, fst <$> sock2MB) of
        (Just sock1, Just sock2) -> pure $ BothSock sock1 sock2
        (Just sock1, Nothing)    -> pure $ IPv4Sock sock1
        (Nothing, Just sock2)    -> pure $ IPv6Sock sock2
        (_, _)                   -> do
            logWarning "Couldn't create both IPv4 and IPv6 socket, retrying in 5 sec..."
            threadDelay 5000000
            mkSockets settings
  where
    logging (_, addrInfo) = logInfo $
        sformat ("Created socket (family/addr): "%shown%"/"%shown)
                (addrFamily addrInfo) (addrAddress addrInfo)
    doMkSockets :: IO (Maybe (Socket, AddrInfo), Maybe (Socket, AddrInfo))
    doMkSockets = do
        serveraddrs <- udpLocalAddresses
        (,) <$> createAndBindSock selectIPv4 serveraddrs
            <*> createAndBindSock selectIPv6 serveraddrs
    handlerE e = do
        logWarning $
            sformat ("Failed to create sockets, retrying in 5 sec... (reason: "%shown%")")
            e
        threadDelay 5000000
        doMkSockets

handleNtpPacket :: NtpClient -> NtpPacket -> IO ()
handleNtpPacket cli packet = do
    logDebug $ sformat ("Got packet "%shown) packet

    clockOffset <- evalClockOffset packet

    logDebug $ sformat ("Received time delta "%shown%" mcs")
        (toMicroseconds clockOffset)

    late <- atomically . modifyTVarS (ncState cli) $ do
        _Just %= ((clockOffset, ntpOriginTime packet) :)
        gets isNothing
    when late $
        logWarning "Response was too late"

doReceive :: Socket -> NtpClient -> IO ()
doReceive sock cli = forever $ do
    (received, _) <- recvFrom sock ntpPacketSize
    let eNtpPacket = decodeOrFail $ LBS.fromStrict received
    case eNtpPacket of
        Left  (_, _, err)    ->
            logWarning $ sformat ("Error while receiving time: "%shown) err
        Right (_, _, packet) ->
            handleNtpPacket cli packet `catchAny` handleE
  where
    handleE = logWarning . sformat ("Error while handle packet: "%shown)

-- |
-- Start listening for responses on the socket `ncSockets
startReceive :: NtpClient -> IO ()
startReceive cli = do
    sockets <- readTVarIO $ ncSockets cli
    case sockets of
        BothSock sIPv4 sIPv6 ->
            () <$ runDoReceive True sIPv4 `concurrently` runDoReceive False sIPv6
        IPv4Sock sIPv4 -> runDoReceive True sIPv4
        IPv6Sock sIPv6 -> runDoReceive False sIPv6
  where
    runDoReceive isIPv4 sock = doReceive sock cli `catchAny` handleE isIPv4 sock
    -- got error while receiving data, retrying in 5 sec
    handleE isIPv4 sock e = do
        logDebug $ sformat ("doReceive failed on socket"%shown%
                            ", reason: "%shown%
                            ", recreate socket in 5 sec") sock e
        threadDelay 5000000
        serveraddrs <- udpLocalAddresses
        newSockMB <-
            if isIPv4 then
                traverse (overwriteSocket IPv4Sock . fst) =<< createAndBindSock selectIPv4 serveraddrs
            else
                traverse (overwriteSocket IPv6Sock . fst) =<< createAndBindSock selectIPv6 serveraddrs
        case newSockMB of
            Nothing      -> logWarning "Recreating of socket failed" >> handleE isIPv4 sock e
            Just newSock -> runDoReceive isIPv4 newSock
    overwriteSocket constr sock = sock <$
        (atomically .
         modifyTVar' (ncSockets cli) .
         flip mergeSockets .
         constr $ sock)

-- |
-- Spawn ntp client which will send request to ntp servers every ntpPollDelay
-- and will lisent for responses.  The `ncStatus` will be updated every
-- `ntpPollDelay` with the most recent value.  It should be run in a seprate
-- thread, since it will block infinitelly.
spawnNtpClient :: NtpClientSettings -> TVar NtpStatus -> IO ()
spawnNtpClient settings ncStatus =
    withSocketsDoLifted $
    bracket (mkSockets settings) closeSockets $ \sock -> do
        cli <- mkNtpClient settings ncStatus sock

        addrs <- catMaybes <$> mapM (resolveHost $ socketsToBoolDescr sock)
                                    (ntpServers settings)
        when (null addrs) $ throwM NoHostResolved
        () <$ startReceive cli `concurrently`
              startSend cli addrs `concurrently`
              logInfo "Launched NTP client"
  where
    closeSockets sockets = do
        logInfo "NTP client is stopped"
        forM_ (socketsToList sockets) close
    resolveHost sockDescr host = do
        maddr <- resolveNtpHost host sockDescr
        case maddr of
            Nothing   -> do
                logWarning $ sformat ("Host "%shown%" is not resolved") host
                pure Nothing
            Just addr -> do
                logInfo $ sformat ("Host "%shown%" is resolved: "%shown) host addr
                pure $ Just addr


-- | Run Ntp client in a seprate thread, return a mutable cell which holds
-- `NtpStatus`.
withNtpClient :: MonadIO m => NtpClientSettings -> m (TVar NtpStatus)
withNtpClient ntpSettings = do
    ntpStatus <- newTVarIO NtpSyncPending
    _ <- liftIO $ async (spawnNtpClient ntpSettings ntpStatus)
    return ntpStatus

-- | Run without Ntp client.
withoutNtpClient :: MonadIO m => (TVar NtpStatus -> m a) -> m a
withoutNtpClient f = newTVarIO NtpSyncUnavailable >>= f

-- | Start client, wait for a while so that most likely it ticks once
-- and stop it.
ntpSingleShot
    :: NtpClientSettings
    -> TVar NtpStatus
    -> IO ()
ntpSingleShot ntpSettings ncStatus =
    () <$ timeout (ntpResponseTimeout ntpSettings) (spawnNtpClient ntpSettings ncStatus)

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

timeout :: TimeUnit t => t -> IO a -> IO (Maybe a)
timeout t io = rightToMaybe <$> race (threadDelay (fromIntegral (toMicroseconds t))) io
